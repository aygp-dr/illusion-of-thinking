.PHONY: download clean diagrams help

PAPER_URL = https://ml-site.cdn-apple.com/papers/the-illusion-of-thinking.pdf
PAPER_FILE = the-illusion-of-thinking.pdf

help:
	@echo "Available targets:"
	@echo "  download  - Download the research paper if not present locally"
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

clean:
	@echo "Removing downloaded paper and generated diagrams..."
	@rm -f $(PAPER_FILE)
	@rm -f diagrams/*.png