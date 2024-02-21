# Usage: set_localization_symlinks_interactive_histograms_app.sh targetLanguage

# Delete existing symlinks
rm interactive_histogram_display.html
rm interactive_histogram_display.labels.txt

# Create new symlinks
ln -s "interactive_histogram_display.${1}.html" "interactive_histogram_display.html"
ln -s "interactive_histogram_display.labels.${1}.txt" "interactive_histogram_display.labels.txt"
