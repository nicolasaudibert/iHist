# Usage: set_localization_symlinks_interactive_histograms_app.sh targetLanguage

# Delete existing symlinks
rm -f iHist.html
rm -f iHist.labels.txt

# Create new symlinks
ln -s "localization/iHist.${1}.html" "iHist.html"
ln -s "localization/iHist.labels.${1}.txt" "iHist.labels.txt"
