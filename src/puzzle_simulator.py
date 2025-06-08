#!/usr/bin/env python3
"""
Puzzle Simulators for LRM Evaluation
Based on 'The Illusion of Thinking' paper by Apple

This module implements simple simulators for the puzzle environments
described in the paper, to demonstrate the complexity thresholds.
"""

import math
from typing import List, Tuple, Dict, Any, Optional


class TowerOfHanoi:
    """Tower of Hanoi puzzle simulator."""

    def __init__(self, n: int):
        """Initialize Tower of Hanoi with n disks."""
        self.n = n
        self.state = [list(range(n, 0, -1)), [], []]
        self.moves = 0
        
    def is_valid_move(self, disk: int, from_peg: int, to_peg: int) -> bool:
        """Check if a move is valid."""
        if from_peg < 0 or from_peg > 2 or to_peg < 0 or to_peg > 2:
            return False
        if from_peg == to_peg:
            return False
        if not self.state[from_peg] or self.state[from_peg][0] != disk:
            return False
        if self.state[to_peg] and self.state[to_peg][0] < disk:
            return False
        return True
        
    def apply_move(self, disk: int, from_peg: int, to_peg: int) -> bool:
        """Apply a move if valid."""
        if not self.is_valid_move(disk, from_peg, to_peg):
            return False
        
        self.state[to_peg].insert(0, disk)
        self.state[from_peg].pop(0)
        self.moves += 1
        return True
        
    def is_solved(self) -> bool:
        """Check if the puzzle is solved."""
        goal_state = [[], [], list(range(self.n, 0, -1))]
        return self.state == goal_state
        
    def min_moves_required(self) -> int:
        """Return the minimum number of moves required."""
        return 2**self.n - 1
        
    def __str__(self) -> str:
        """Return string representation of the current state."""
        result = []
        result.append(f"Tower of Hanoi (n={self.n})")
        result.append(f"Minimum moves required: {self.min_moves_required()}")
        result.append(f"Current moves: {self.moves}")
        
        # Represent the towers
        max_height = self.n
        tower_strs = []
        
        for peg in range(3):
            tower = ["   |   " for _ in range(max_height - len(self.state[peg]))]
            for disk in self.state[peg]:
                width = disk * 2 - 1
                tower.append(f"{'-' * width:^7}")
            tower_strs.append(tower)
            
        # Combine the towers side by side
        for i in range(max_height):
            row = "   ".join(tower[i] for tower in tower_strs)
            result.append(row)
            
        result.append("=" * 25)
        return "\n".join(result)


class CheckerJumping:
    """Checker jumping puzzle simulator."""
    
    def __init__(self, n: int):
        """Initialize with n red and n blue checkers."""
        self.n = n
        # Red checkers on left, blank in middle, blue on right
        self.state = ['R'] * n + ['_'] + ['B'] * n
        self.moves = 0
        
    def is_valid_move(self, color: str, from_pos: int, to_pos: int) -> bool:
        """Check if a checker move is valid."""
        if from_pos < 0 or from_pos >= len(self.state) or to_pos < 0 or to_pos >= len(self.state):
            return False
            
        if self.state[from_pos] != color or self.state[to_pos] != '_':
            return False
            
        distance = to_pos - from_pos
        if color == 'R' and distance <= 0:  # Red moves right
            return False
        if color == 'B' and distance >= 0:  # Blue moves left
            return False
            
        abs_distance = abs(distance)
        if abs_distance == 1:  # Slide
            return True
        elif abs_distance == 2:  # Jump
            middle_pos = (from_pos + to_pos) // 2
            middle_color = self.state[middle_pos]
            return middle_color != '_' and middle_color != color
        else:
            return False
            
    def apply_move(self, color: str, from_pos: int, to_pos: int) -> bool:
        """Apply a move if valid."""
        if not self.is_valid_move(color, from_pos, to_pos):
            return False
            
        self.state[to_pos] = color
        self.state[from_pos] = '_'
        self.moves += 1
        return True
        
    def is_solved(self) -> bool:
        """Check if the puzzle is solved."""
        goal_state = ['B'] * self.n + ['_'] + ['R'] * self.n
        return self.state == goal_state
        
    def min_moves_required(self) -> int:
        """Return the minimum number of moves required (approximation)."""
        return (self.n + 1)**2 - 1
        
    def __str__(self) -> str:
        """Return string representation of the current state."""
        result = []
        result.append(f"Checker Jumping (n={self.n})")
        result.append(f"Minimum moves required: {self.min_moves_required()}")
        result.append(f"Current moves: {self.moves}")
        
        # Represent the board
        board = "  " + "  ".join(str(i) for i in range(len(self.state)))
        result.append(board)
        
        checkers = "[ " + " ".join(self.state) + " ]"
        result.append(checkers)
        
        result.append("=" * 25)
        return "\n".join(result)


def simulate_lrm_accuracy(complexity: int, puzzle_type: str, model_type: str) -> float:
    """
    Simulate LRM accuracy based on complexity level.
    
    Args:
        complexity: Problem complexity (size/n)
        puzzle_type: Type of puzzle (tower-hanoi, checker-jumping, etc.)
        model_type: 'thinking' or 'standard'
        
    Returns:
        Estimated accuracy percentage
    """
    # Different collapse points for different puzzles
    base_threshold = {
        'tower-hanoi': 10,
        'checker-jumping': 6,
        'river-crossing': 4,
        'blocks-world': 8
    }.get(puzzle_type, 8)
    
    thinking_bonus = 2 if model_type == 'thinking' else 0
    threshold = base_threshold + thinking_bonus
    
    # Low complexity regime - standard models are better
    if complexity < 3:
        if model_type == 'thinking':
            return max(0, 100 - complexity * 10)  # Thinking models overthink
        else:
            return max(0, 100 - complexity * 5)   # Standard models more efficient
    
    # Medium complexity regime - thinking models excel
    elif complexity < threshold:
        if model_type == 'thinking':
            return max(0, 100 - complexity * 8)
        else:
            return max(0, 100 - complexity * 15)
    
    # High complexity regime - both collapse
    else:
        return 0


def simulate_thinking_tokens(complexity: int, puzzle_type: str) -> int:
    """
    Simulate the number of thinking tokens used based on complexity.
    Demonstrates the counterintuitive scaling limit.
    """
    peak_complexity = {
        'tower-hanoi': 8,
        'checker-jumping': 5,
        'river-crossing': 3,
        'blocks-world': 6
    }.get(puzzle_type, 6)
    
    # Initial scaling with complexity
    if complexity < peak_complexity:
        return complexity * complexity * 1000
    
    # Counterintuitive decline after threshold
    elif complexity < peak_complexity * 2:
        return int((peak_complexity * 2 - complexity) * 800)
    
    # Minimal effort at high complexity
    else:
        return 1000


def demonstrate_regimes(puzzle_type: str = 'tower-hanoi') -> str:
    """
    Demonstrate the three performance regimes for a given puzzle type.
    Returns a formatted string with the results.
    """
    result = []
    result.append(f"=== LRM Performance Regimes Demo: {puzzle_type} ===\n")
    result.append("N\tStandard\tThinking\tTokens")
    result.append("-" * 50)
    
    for n in [1, 2, 3, 4, 5, 6, 7, 8, 10, 12, 15]:
        standard_acc = simulate_lrm_accuracy(n, puzzle_type, 'standard')
        thinking_acc = simulate_lrm_accuracy(n, puzzle_type, 'thinking')
        tokens = simulate_thinking_tokens(n, puzzle_type)
        
        result.append(f"{n}\t{standard_acc:.1f}%\t\t{thinking_acc:.1f}%\t\t{tokens}")
    
    return "\n".join(result)


def run_demo() -> None:
    """Run a simple demonstration of the concepts."""
    print("\n=== 'The Illusion of Thinking' Puzzle Demonstrations ===\n")
    
    # Tower of Hanoi example
    hanoi = TowerOfHanoi(3)
    print(hanoi)
    
    # Show some moves
    moves = [(1, 0, 2), (2, 0, 1), (1, 2, 1), (3, 0, 2)]
    for disk, from_peg, to_peg in moves:
        success = hanoi.apply_move(disk, from_peg, to_peg)
        print(f"Move disk {disk} from peg {from_peg} to peg {to_peg}: {'Success' if success else 'Failed'}")
    
    print(hanoi)
    
    # Checker jumping example
    checkers = CheckerJumping(2)
    print(checkers)
    
    # Show some moves
    moves = [('R', 0, 2), ('B', 4, 3), ('R', 1, 4)]
    for color, from_pos, to_pos in moves:
        success = checkers.apply_move(color, from_pos, to_pos)
        print(f"Move {color} from {from_pos} to {to_pos}: {'Success' if success else 'Failed'}")
    
    print(checkers)
    
    # Performance regime demonstration
    print("\n" + demonstrate_regimes('tower-hanoi'))
    print("\n" + demonstrate_regimes('checker-jumping'))


if __name__ == "__main__":
    run_demo()