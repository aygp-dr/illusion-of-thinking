.PHONY: download clean diagrams run extract tutorial help

PAPER_URL = https://ml-site.cdn-apple.com/papers/the-illusion-of-thinking.pdf
PAPER_FILE = the-illusion-of-thinking.pdf
PAPER_TEXT = paper/the-illusion-of-thinking.txt
PAPER_DIR = paper

help:
	@echo "Available targets:"
	@echo "  download  - Download the research paper if not present locally"
	@echo "  extract   - Extract text from the PDF paper for analysis"
	@echo "  tutorial  - Run a tutorial on key concepts from the paper"
	@echo "  run       - Run the puzzle simulator examples"
	@echo "  diagrams  - Convert Mermaid diagram files (.mmd) to PNG images"
	@echo "  clean     - Remove the downloaded paper and generated diagrams"
	@echo "  help      - Display this help message"

download:
	@if [ ! -f $(PAPER_FILE) ]; then \
		echo "Downloading $(PAPER_FILE)..."; \
		wget -q --show-progress $(PAPER_URL) -O $(PAPER_FILE); \
		echo "Download complete."; \
	else \
		echo "File $(PAPER_FILE) already exists."; \
	fi

%.png: %.mmd
	@echo "Converting $< to $@..."
	@mkdir -p $(dir $@)
	@mmdc -i $< -o $@ -b transparent

diagrams: $(patsubst %.mmd,%.png,$(wildcard diagrams/*.mmd))
	@echo "All diagrams converted to PNG."

run:
	@echo "Running puzzle simulators..."
	@python3 src/puzzle_simulator.py

extract: download
	@echo "Extracting text from $(PAPER_FILE)..."
	@mkdir -p $(PAPER_DIR)
	@pdftotext -layout $(PAPER_FILE) $(PAPER_TEXT)
	@echo "Text extracted to $(PAPER_TEXT)"

tutorial: extract
	@echo "Running paper tutorial..."
	@guile src/paper-tutorial.scm

clean:
	@echo "Removing downloaded paper, extracted text, and generated diagrams..."
	@rm -f $(PAPER_FILE)
	@rm -rf $(PAPER_DIR)
	@rm -f diagrams/*.png