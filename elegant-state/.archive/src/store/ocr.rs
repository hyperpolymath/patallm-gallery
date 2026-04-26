//! OCR support using Tesseract
//!
//! Provides text extraction from images for indexing into the state graph.

use std::process::{Command, Stdio};
use std::path::Path;
use crate::store::StoreError;

/// Supported OCR languages
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OcrLanguage {
    English,
    German,
    French,
    Spanish,
    Italian,
    Portuguese,
    Dutch,
    Russian,
    Japanese,
    Chinese,
    Korean,
    Arabic,
    Custom(&'static str),
}

impl OcrLanguage {
    fn as_tesseract_arg(self) -> &'static str {
        match self {
            OcrLanguage::English => "eng",
            OcrLanguage::German => "deu",
            OcrLanguage::French => "fra",
            OcrLanguage::Spanish => "spa",
            OcrLanguage::Italian => "ita",
            OcrLanguage::Portuguese => "por",
            OcrLanguage::Dutch => "nld",
            OcrLanguage::Russian => "rus",
            OcrLanguage::Japanese => "jpn",
            OcrLanguage::Chinese => "chi_sim",
            OcrLanguage::Korean => "kor",
            OcrLanguage::Arabic => "ara",
            OcrLanguage::Custom(code) => code,
        }
    }
}

/// OCR engine modes
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum OcrEngineMode {
    /// Legacy engine only
    Legacy,
    /// LSTM neural network only
    LstmOnly,
    /// Both legacy and LSTM
    #[default]
    Combined,
    /// Default, based on availability
    Default,
}

impl OcrEngineMode {
    fn as_tesseract_arg(self) -> &'static str {
        match self {
            OcrEngineMode::Legacy => "0",
            OcrEngineMode::LstmOnly => "1",
            OcrEngineMode::Combined => "2",
            OcrEngineMode::Default => "3",
        }
    }
}

/// Page segmentation modes
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum PageSegMode {
    /// Automatic page segmentation with OSD
    #[default]
    Auto,
    /// Assume single column of text
    SingleColumn,
    /// Assume single block of text
    SingleBlock,
    /// Assume single line of text
    SingleLine,
    /// Assume single word
    SingleWord,
    /// Sparse text
    SparseText,
}

impl PageSegMode {
    fn as_tesseract_arg(self) -> &'static str {
        match self {
            PageSegMode::Auto => "3",
            PageSegMode::SingleColumn => "4",
            PageSegMode::SingleBlock => "6",
            PageSegMode::SingleLine => "7",
            PageSegMode::SingleWord => "8",
            PageSegMode::SparseText => "11",
        }
    }
}

/// Tesseract-based OCR engine
pub struct OcrEngine {
    tesseract_path: String,
    language: OcrLanguage,
    engine_mode: OcrEngineMode,
    page_seg_mode: PageSegMode,
}

impl Default for OcrEngine {
    fn default() -> Self {
        Self::new()
    }
}

impl OcrEngine {
    /// Create a new OCR engine with default settings
    pub fn new() -> Self {
        Self {
            tesseract_path: "tesseract".to_string(),
            language: OcrLanguage::English,
            engine_mode: OcrEngineMode::default(),
            page_seg_mode: PageSegMode::default(),
        }
    }

    /// Set the tesseract binary path
    pub fn with_path(mut self, path: impl Into<String>) -> Self {
        self.tesseract_path = path.into();
        self
    }

    /// Set the OCR language
    pub fn with_language(mut self, lang: OcrLanguage) -> Self {
        self.language = lang;
        self
    }

    /// Set the engine mode
    pub fn with_engine_mode(mut self, mode: OcrEngineMode) -> Self {
        self.engine_mode = mode;
        self
    }

    /// Set the page segmentation mode
    pub fn with_page_seg_mode(mut self, mode: PageSegMode) -> Self {
        self.page_seg_mode = mode;
        self
    }

    /// Check if tesseract is available
    pub fn is_available(&self) -> bool {
        Command::new(&self.tesseract_path)
            .arg("--version")
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .status()
            .is_ok_and(|s| s.success())
    }

    /// Get tesseract version
    pub fn version(&self) -> Result<String, StoreError> {
        let output = Command::new(&self.tesseract_path)
            .arg("--version")
            .stderr(Stdio::piped()) // tesseract outputs version to stderr
            .output()
            .map_err(|e| StoreError::Serialization(format!("tesseract not found: {e}")))?;

        let version = String::from_utf8_lossy(&output.stderr);
        Ok(version.lines().next().unwrap_or("unknown").to_string())
    }

    /// List available languages
    pub fn list_languages(&self) -> Result<Vec<String>, StoreError> {
        let output = Command::new(&self.tesseract_path)
            .arg("--list-langs")
            .output()
            .map_err(|e| StoreError::Serialization(format!("tesseract failed: {e}")))?;

        let stdout = String::from_utf8_lossy(&output.stdout);
        let languages: Vec<String> = stdout
            .lines()
            .skip(1) // Skip header line
            .map(|s| s.trim().to_string())
            .filter(|s| !s.is_empty())
            .collect();

        Ok(languages)
    }

    /// Extract text from an image file
    pub fn extract_text<P: AsRef<Path>>(&self, image_path: P) -> Result<String, StoreError> {
        let path = image_path.as_ref();

        if !path.exists() {
            return Err(StoreError::Serialization(format!(
                "image file not found: {}",
                path.display()
            )));
        }

        let output = Command::new(&self.tesseract_path)
            .arg(path)
            .arg("stdout") // Output to stdout instead of file
            .args(["-l", self.language.as_tesseract_arg()])
            .args(["--oem", self.engine_mode.as_tesseract_arg()])
            .args(["--psm", self.page_seg_mode.as_tesseract_arg()])
            .output()
            .map_err(|e| StoreError::Serialization(format!("tesseract failed: {e}")))?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(StoreError::Serialization(format!("OCR error: {stderr}")));
        }

        Ok(String::from_utf8_lossy(&output.stdout).into_owned())
    }

    /// Extract text from image bytes (creates temp file)
    pub fn extract_text_from_bytes(&self, bytes: &[u8], extension: &str) -> Result<String, StoreError> {
        use std::io::Write;

        let temp_dir = std::env::temp_dir();
        let temp_path = temp_dir.join(format!("ocr_temp_{}.{}", ulid::Ulid::new(), extension));

        // Write bytes to temp file
        let mut file = std::fs::File::create(&temp_path)
            .map_err(|e| StoreError::Serialization(format!("failed to create temp file: {e}")))?;
        file.write_all(bytes)
            .map_err(|e| StoreError::Serialization(format!("failed to write temp file: {e}")))?;

        // Extract text
        let result = self.extract_text(&temp_path);

        // Clean up
        let _ = std::fs::remove_file(&temp_path);

        result
    }

    /// Extract text with confidence scores (returns HOCR format)
    pub fn extract_with_confidence<P: AsRef<Path>>(&self, image_path: P) -> Result<String, StoreError> {
        let path = image_path.as_ref();

        let output = Command::new(&self.tesseract_path)
            .arg(path)
            .arg("stdout")
            .args(["-l", self.language.as_tesseract_arg()])
            .args(["--oem", self.engine_mode.as_tesseract_arg()])
            .args(["--psm", self.page_seg_mode.as_tesseract_arg()])
            .arg("hocr") // Output HOCR format with confidence
            .output()
            .map_err(|e| StoreError::Serialization(format!("tesseract failed: {e}")))?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(StoreError::Serialization(format!("OCR error: {stderr}")));
        }

        Ok(String::from_utf8_lossy(&output.stdout).into_owned())
    }
}

/// Detect if a file is likely an image based on extension
pub fn is_image_file(path: &str) -> bool {
    let ext = path.rsplit('.').next().unwrap_or("").to_lowercase();
    matches!(ext.as_str(), "png" | "jpg" | "jpeg" | "tiff" | "tif" | "bmp" | "gif" | "webp")
}

/// Supported image formats for OCR
pub fn supported_formats() -> &'static [&'static str] {
    &["png", "jpg", "jpeg", "tiff", "tif", "bmp", "gif", "webp", "pnm", "pbm", "pgm", "ppm"]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_image_file() {
        assert!(is_image_file("photo.png"));
        assert!(is_image_file("scan.JPEG"));
        assert!(is_image_file("doc.tiff"));
        assert!(!is_image_file("document.pdf"));
        assert!(!is_image_file("text.txt"));
    }

    #[test]
    fn test_ocr_available() {
        let ocr = OcrEngine::new();
        if ocr.is_available() {
            let version = ocr.version().unwrap();
            assert!(version.contains("tesseract"));
        }
    }

    #[test]
    fn test_list_languages() {
        let ocr = OcrEngine::new();
        if ocr.is_available() {
            let langs = ocr.list_languages().unwrap();
            // eng should be available in most installations
            assert!(langs.iter().any(|l| l == "eng"));
        }
    }
}
