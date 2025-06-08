.PHONY: download clean help

PAPER_URL = https://ml-site.cdn-apple.com/papers/the-illusion-of-thinking.pdf
PAPER_FILE = the-illusion-of-thinking.pdf

help:
	@echo "Available targets:"
	@echo "  download  - Download the research paper if not present locally"
	@echo "  clean     - Remove the downloaded research paper"
	@echo "  help      - Display this help message"

download:
	@if [ ! -f $(PAPER_FILE) ]; then \
		echo "Downloading $(PAPER_FILE)..."; \
		wget -q --show-progress $(PAPER_URL) -O $(PAPER_FILE); \
		echo "Download complete."; \
	else \
		echo "File $(PAPER_FILE) already exists."; \
	fi

clean:
	@echo "Removing downloaded paper..."
	@rm -f $(PAPER_FILE)