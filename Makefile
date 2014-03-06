VERSION = 0.4.2
TARGET_DIR = howdoi-$(VERSION)

marmalade:
	mkdir $(TARGET_DIR)
	cp howdoi.el $(TARGET_DIR)
	cp README.md $(TARGET_DIR)
	cp LICENSE $(TARGET_DIR)
	tar -cf howdoi-$(VERSION).tar $(TARGET_DIR)
	rm -rf $(TARGET_DIR)
