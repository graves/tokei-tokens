use std::{
    borrow::Cow,
    fmt,
    io::{self, Write},
    process,
    str::FromStr,
};

use clap::crate_version;
// Avoid ANSI in width-constrained fields.
// use colored::Colorize;
use num_format::ToFormattedString;

use crate::input::Format;
use tokei-tokens::{CodeStats, Language, LanguageType, Report, find_char_boundary};

use crate::consts::{
    BLANKS_COLUMN_WIDTH, CODE_COLUMN_WIDTH, COMMENT_TOKENS_COLUMN_WIDTH, COMMENTS_COLUMN_WIDTH,
    FILES_COLUMN_WIDTH, LINES_COLUMN_WIDTH, SOURCE_TOKENS_COLUMN_WIDTH, TOTAL_TOKENS_COLUMN_WIDTH,
};

#[inline]
fn files_col_width() -> usize {
    // Must match Files column width in header.
    FILES_COLUMN_WIDTH + 6
}

const IDENT_INACCURATE: &str = "(!)";

#[inline]
fn tail_width_for_table() -> usize {
    // Width to the RIGHT of the first column (Language/filepath),
    // including the single separating space before the numeric block.
    let files_w = files_col_width();
    1 + files_w
        + 1
        + LINES_COLUMN_WIDTH
        + 1
        + CODE_COLUMN_WIDTH
        + 1
        + COMMENTS_COLUMN_WIDTH
        + 1
        + BLANKS_COLUMN_WIDTH
        + 1
        + SOURCE_TOKENS_COLUMN_WIDTH
        + 1
        + COMMENT_TOKENS_COLUMN_WIDTH
        + 1
        + TOTAL_TOKENS_COLUMN_WIDTH
}

pub fn crate_version() -> String {
    if Format::supported().is_empty() {
        format!(
            "{} compiled without serialization formats.",
            crate_version!()
        )
    } else {
        format!(
            "{} compiled with serialization support: {}",
            crate_version!(),
            Format::supported().join(", ")
        )
    }
}

pub fn setup_logger(verbose_option: u64) {
    use log::LevelFilter;

    let mut builder = env_logger::Builder::new();

    let filter_level = match verbose_option {
        1 => LevelFilter::Warn,
        2 => LevelFilter::Debug,
        3 => LevelFilter::Trace,
        _ => LevelFilter::Error,
    };

    builder.filter(None, filter_level);
    builder.init();
}

pub fn parse_or_exit<T>(s: &str) -> T
where
    T: FromStr,
    T::Err: fmt::Display,
{
    T::from_str(s).unwrap_or_else(|e| {
        eprintln!("Error:\n{}", e);
        process::exit(1);
    })
}

#[non_exhaustive]
#[derive(Debug, Copy, Clone)]
pub enum NumberFormatStyle {
    Plain,       // 1234 (Default)
    Commas,      // 1,234
    Dots,        // 1.234
    Underscores, // 1_234
}

impl Default for NumberFormatStyle {
    fn default() -> Self {
        Self::Plain
    }
}

impl FromStr for NumberFormatStyle {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "plain" => Ok(Self::Plain),
            "commas" => Ok(Self::Commas),
            "dots" => Ok(Self::Dots),
            "underscores" => Ok(Self::Underscores),
            _ => Err(format!(
                "Expected 'plain', 'commas', 'underscores', or 'dots' for num-format, but got '{}'",
                s,
            )),
        }
    }
}

impl NumberFormatStyle {
    fn separator(self) -> &'static str {
        match self {
            Self::Plain => "",
            Self::Commas => ",",
            Self::Dots => ".",
            Self::Underscores => "_",
        }
    }

    pub fn get_format(self) -> Result<num_format::CustomFormat, num_format::Error> {
        num_format::CustomFormat::builder()
            .grouping(num_format::Grouping::Standard)
            .separator(self.separator())
            .build()
    }
}

pub struct Printer<W> {
    writer: W,
    /// Terminal columns (hint from main).
    columns: usize,
    /// Fixed width of the first (left) column: Language / filepath.
    first_col_width: usize,
    /// Exact width of the whole rendered table (left col + numeric tail).
    table_width: usize,
    /// For file rows path truncation (same as first_col_width).
    path_length: usize,
    row: String,
    subrow: String,
    list_files: bool,
    number_format: num_format::CustomFormat,
}

impl<W> Printer<W> {
    pub fn new(
        columns: usize,
        list_files: bool,
        writer: W,
        number_format: num_format::CustomFormat,
    ) -> Self {
        let numeric_tail = tail_width_for_table();

        // Ensure the Language column never collapses below a reasonable width.
        // 12 works well for common language names and keeps the numeric block aligned.
        const MIN_FIRST_COL: usize = 12;

        // If the terminal is narrow, we *still* keep MIN_FIRST_COL for alignment, even
        // if that means the table will be wider than `columns` (and may soft-wrap in the terminal).
        let first_col_width = std::cmp::max(columns.saturating_sub(numeric_tail), MIN_FIRST_COL);

        // Build the exact table width from our specs, not the terminal columns.
        let table_width = first_col_width + numeric_tail;

        Self {
            columns,
            list_files,
            first_col_width,
            table_width,
            path_length: first_col_width, // used for file/path truncation
            writer,
            row: "━".repeat(table_width),
            subrow: "─".repeat(table_width),
            number_format,
        }
    }
}

impl<W: Write> Printer<W> {
    pub fn print_header(&mut self) -> io::Result<()> {
        self.print_row()?;

        let files_column_width: usize = files_col_width();

        writeln!(
            self.writer,
            "{:<lang_w$} \
             {:>files_column_width$} \
             {:>LINES_COLUMN_WIDTH$} \
             {:>CODE_COLUMN_WIDTH$} \
             {:>COMMENTS_COLUMN_WIDTH$} \
             {:>BLANKS_COLUMN_WIDTH$} \
             {:>SOURCE_TOKENS_COLUMN_WIDTH$} \
             {:>COMMENT_TOKENS_COLUMN_WIDTH$} \
             {:>TOTAL_TOKENS_COLUMN_WIDTH$}",
            "Language",
            "Files",
            "Lines",
            "Code",
            "Comments",
            "Blanks",
            "SrcTok",
            "ComTok",
            "TotTok",
            lang_w = self.first_col_width,
        )?;
        self.print_row()
    }

    pub fn print_inaccuracy_warning(&mut self) -> io::Result<()> {
        writeln!(
            self.writer,
            "Note: results can be inaccurate for languages marked with '{}'",
            IDENT_INACCURATE
        )
    }

    pub fn print_language(&mut self, language: &Language, name: &str) -> io::Result<()>
    where
        W: Write,
    {
        let files_column_width = files_col_width();

        let label = self.render_first_col_label(name, language.inaccurate);

        // Token totals: sum file summaries
        let (src_tok, com_tok) = language
            .reports
            .iter()
            .map(|r| r.stats.summarise())
            .fold((0usize, 0usize), |(a_src, a_com), s| {
                (a_src + s.source_code_tokens, a_com + s.comment_tokens)
            });
        let tot_tok = src_tok + com_tok;

        writeln!(
            self.writer,
            "{:<lang_w$} \
             {:>files_column_width$} \
             {:>LINES_COLUMN_WIDTH$} \
             {:>CODE_COLUMN_WIDTH$} \
             {:>COMMENTS_COLUMN_WIDTH$} \
             {:>BLANKS_COLUMN_WIDTH$} \
             {:>SOURCE_TOKENS_COLUMN_WIDTH$} \
             {:>COMMENT_TOKENS_COLUMN_WIDTH$} \
             {:>TOTAL_TOKENS_COLUMN_WIDTH$}",
            label,
            language
                .reports
                .len()
                .to_formatted_string(&self.number_format),
            language.lines().to_formatted_string(&self.number_format),
            language.code.to_formatted_string(&self.number_format),
            language.comments.to_formatted_string(&self.number_format),
            language.blanks.to_formatted_string(&self.number_format),
            src_tok.to_formatted_string(&self.number_format),
            com_tok.to_formatted_string(&self.number_format),
            tot_tok.to_formatted_string(&self.number_format),
            lang_w = self.first_col_width,
        )
    }

    fn print_language_in_print_total(&mut self, language: &Language) -> io::Result<()>
    where
        W: Write,
    {
        let files_column_width = files_col_width();

        let label = self.render_first_col_label("Total", language.inaccurate);

        // Aggregate across children
        let (files_total, lines_total, src_tok, com_tok) =
            language
                .children
                .values()
                .fold((0usize, 0usize, 0usize, 0usize), |acc, reps| {
                    let (mut f, mut lines, mut src, mut com) = acc;
                    f += reps.len();
                    for r in reps {
                        let s = r.stats.summarise();
                        lines += s.lines();
                        src += s.source_code_tokens;
                        com += s.comment_tokens;
                    }
                    (f, lines, src, com)
                });
        let tot_tok = src_tok + com_tok;

        writeln!(
            self.writer,
            "{:<lang_w$} \
             {:>files_column_width$} \
             {:>LINES_COLUMN_WIDTH$} \
             {:>CODE_COLUMN_WIDTH$} \
             {:>COMMENTS_COLUMN_WIDTH$} \
             {:>BLANKS_COLUMN_WIDTH$} \
             {:>SOURCE_TOKENS_COLUMN_WIDTH$} \
             {:>COMMENT_TOKENS_COLUMN_WIDTH$} \
             {:>TOTAL_TOKENS_COLUMN_WIDTH$}",
            label,
            files_total.to_formatted_string(&self.number_format),
            lines_total.to_formatted_string(&self.number_format),
            language.code.to_formatted_string(&self.number_format),
            language.comments.to_formatted_string(&self.number_format),
            language.blanks.to_formatted_string(&self.number_format),
            src_tok.to_formatted_string(&self.number_format),
            com_tok.to_formatted_string(&self.number_format),
            tot_tok.to_formatted_string(&self.number_format),
            lang_w = self.first_col_width,
        )
    }

    /// Render a label into the first column width, with optional inaccuracy marker
    /// and safe truncation that leaves a leading '|' if truncated from the left.
    fn render_first_col_label(&self, name: &str, inaccurate: bool) -> String {
        let mut label = name.to_string();
        if inaccurate && self.first_col_width >= IDENT_INACCURATE.len() + 1 {
            label.push_str(IDENT_INACCURATE);
        }
        self.truncate_name_for_width(&label, self.first_col_width)
    }

    pub fn print_language_name(
        &mut self,
        _inaccurate: bool,
        name: &str,
        prefix: Option<&str>,
    ) -> io::Result<()> {
        // Used for embedded language rows (" |-")
        let prefix_len = prefix.map_or(0, str::len);
        let mut width = self.first_col_width.saturating_sub(prefix_len);

        if let Some(prefix) = prefix {
            write!(self.writer, "{}", prefix)?;
        }

        if width == 0 {
            write!(self.writer, "|")?;
        } else if width == 1 {
            write!(self.writer, "|")?;
        } else {
            let rendered = self.truncate_name_for_width(name, width);
            write!(self.writer, "{:<w$}", rendered, w = width)?;
        }

        Ok(())
    }

    fn print_code_stats(
        &mut self,
        language_type: LanguageType,
        stats: &[CodeStats],
    ) -> io::Result<()> {
        // First column for embedded language name: "|-<name>"
        self.print_language_name(false, &language_type.to_string(), Some(" |-"))?;
        write!(self.writer, " ")?; // single space before numerics

        // Aggregate per embedded language
        let mut code = 0usize;
        let mut comments = 0usize;
        let mut blanks = 0usize;
        let mut src_tok = 0usize;
        let mut com_tok = 0usize;

        for s in stats.iter().map(tokei::CodeStats::summarise) {
            code += s.code;
            comments += s.comments;
            blanks += s.blanks;
            src_tok += s.source_code_tokens;
            com_tok += s.comment_tokens;
        }
        let tot_tok = src_tok + com_tok;

        let files_column_width = files_col_width();
        if stats.is_empty() {
            return Ok(());
        }
        writeln!(
            self.writer,
            "{:>files_column_width$} \
             {:>LINES_COLUMN_WIDTH$} \
             {:>CODE_COLUMN_WIDTH$} \
             {:>COMMENTS_COLUMN_WIDTH$} \
             {:>BLANKS_COLUMN_WIDTH$} \
             {:>SOURCE_TOKENS_COLUMN_WIDTH$} \
             {:>COMMENT_TOKENS_COLUMN_WIDTH$} \
             {:>TOTAL_TOKENS_COLUMN_WIDTH$}",
            stats.len().to_formatted_string(&self.number_format),
            (code + comments + blanks).to_formatted_string(&self.number_format),
            code.to_formatted_string(&self.number_format),
            comments.to_formatted_string(&self.number_format),
            blanks.to_formatted_string(&self.number_format),
            src_tok.to_formatted_string(&self.number_format),
            com_tok.to_formatted_string(&self.number_format),
            tot_tok.to_formatted_string(&self.number_format),
        )
    }

    fn print_language_total(&mut self, parent: &Language) -> io::Result<()> {
        for (language, reports) in &parent.children {
            self.print_code_stats(
                *language,
                &reports
                    .iter()
                    .map(|r| r.stats.summarise())
                    .collect::<Vec<_>>(),
            )?;
        }
        let mut subtotal = tokei::Report::new("(Total)".into());
        let summary = parent.summarise();
        subtotal.stats.code += summary.code;
        subtotal.stats.comments += summary.comments;
        subtotal.stats.blanks += summary.blanks;

        self.print_report_with_name(&subtotal)?;
        Ok(())
    }

    pub fn print_results<'a, I>(
        &mut self,
        languages: I,
        compact: bool,
        is_sorted: bool,
    ) -> io::Result<()>
    where
        I: Iterator<Item = (&'a LanguageType, &'a Language)>,
    {
        let (a, b): (Vec<_>, Vec<_>) = languages
            .filter(|(_, v)| !v.is_empty())
            .partition(|(_, l)| compact || l.children.is_empty());
        let mut first = true;

        for languages in &[&a, &b] {
            for &(name, language) in *languages {
                let has_children = !(compact || language.children.is_empty());
                if first {
                    first = false;
                } else if has_children || self.list_files {
                    self.print_subrow()?;
                }

                self.print_language(language, name.name())?;
                if has_children {
                    self.print_language_total(language)?;
                }

                if self.list_files {
                    self.print_subrow()?;
                    let mut reports: Vec<&Report> = language.reports.iter().collect();
                    if !is_sorted {
                        reports.sort_by(|&a, &b| a.name.cmp(&b.name));
                    }
                    if compact {
                        for &report in &reports {
                            self.print_file_row(report)?;
                        }
                    } else {
                        let (a, b): (Vec<&Report>, Vec<&Report>) =
                            reports.iter().partition(|&r| r.stats.blobs.is_empty());
                        for reports in &[&a, &b] {
                            let mut first = true;
                            for report in reports.iter() {
                                if report.stats.blobs.is_empty() {
                                    self.print_file_row(report)?;
                                } else {
                                    if first && a.is_empty() {
                                        writeln!(self.writer, " {}", report.name.display())?;
                                        first = false;
                                    } else {
                                        let name_str = report.name.display().to_string();
                                        let dash_count =
                                            self.table_width.saturating_sub(4 + name_str.len());
                                        writeln!(
                                            self.writer,
                                            "-- {} {}",
                                            name_str,
                                            "-".repeat(dash_count)
                                        )?;
                                    }
                                    self.print_file_row(report)?;
                                    self.print_report_total(report, language.inaccurate)?;
                                }
                            }
                        }
                    }
                }
            }
        }

        Ok(())
    }

    fn print_row(&mut self) -> io::Result<()> {
        // Use table_width so separators always match the actual table.
        if self.row.len() != self.table_width {
            self.row = "━".repeat(self.table_width);
        }
        writeln!(self.writer, "{}", self.row)
    }

    fn print_subrow(&mut self) -> io::Result<()> {
        if self.subrow.len() != self.table_width {
            self.subrow = "─".repeat(self.table_width);
        }
        writeln!(self.writer, "{}", self.subrow)
    }

    fn truncate_name_for_width(&self, s: &str, width: usize) -> String {
        if width == 0 || s.len() <= width {
            return s.to_string();
        }
        // Left-truncate with a leading '|' so the right side remains visible.
        let from = find_char_boundary(s, s.len() + 1 - width);
        format!("|{}", &s[from..])
    }

    fn print_file_row(&mut self, report: &Report) -> io::Result<()> {
        let files_column_width = files_col_width();

        // Fixed width for the name/path column (matches header/first_col_width).
        let raw_name = report.name.to_string_lossy();
        let name = self.truncate_name_for_width(&raw_name, self.first_col_width);

        let name_width = self.first_col_width;

        let stats = &report.stats;
        let tot_tok = stats.source_code_tokens + stats.comment_tokens;

        writeln!(
            self.writer,
            "{: <name_width$} \
             {:>files_column_width$} \
             {:>LINES_COLUMN_WIDTH$} \
             {:>CODE_COLUMN_WIDTH$} \
             {:>COMMENTS_COLUMN_WIDTH$} \
             {:>BLANKS_COLUMN_WIDTH$} \
             {:>SOURCE_TOKENS_COLUMN_WIDTH$} \
             {:>COMMENT_TOKENS_COLUMN_WIDTH$} \
             {:>TOTAL_TOKENS_COLUMN_WIDTH$}",
            name,
            "1", // each file row corresponds to one file
            stats.lines().to_formatted_string(&self.number_format),
            stats.code.to_formatted_string(&self.number_format),
            stats.comments.to_formatted_string(&self.number_format),
            stats.blanks.to_formatted_string(&self.number_format),
            stats
                .source_code_tokens
                .to_formatted_string(&self.number_format),
            stats
                .comment_tokens
                .to_formatted_string(&self.number_format),
            tot_tok.to_formatted_string(&self.number_format),
        )
    }

    fn print_report(
        &mut self,
        language_type: LanguageType,
        stats: &CodeStats,
        inaccurate: bool,
    ) -> io::Result<()> {
        self.print_language_name(inaccurate, &language_type.to_string(), Some(" |-"))?;
        write!(self.writer, " ")?; // single space before numerics

        let tot_tok = stats.source_code_tokens + stats.comment_tokens;
        let files_column_width = files_col_width();

        writeln!(
            self.writer,
            "{:>files_column_width$} \
             {:>LINES_COLUMN_WIDTH$} \
             {:>CODE_COLUMN_WIDTH$} \
             {:>COMMENTS_COLUMN_WIDTH$} \
             {:>BLANKS_COLUMN_WIDTH$} \
             {:>SOURCE_TOKENS_COLUMN_WIDTH$} \
             {:>COMMENT_TOKENS_COLUMN_WIDTH$} \
             {:>TOTAL_TOKENS_COLUMN_WIDTH$}",
            "",
            stats.lines().to_formatted_string(&self.number_format),
            stats.code.to_formatted_string(&self.number_format),
            stats.comments.to_formatted_string(&self.number_format),
            stats.blanks.to_formatted_string(&self.number_format),
            stats
                .source_code_tokens
                .to_formatted_string(&self.number_format),
            stats
                .comment_tokens
                .to_formatted_string(&self.number_format),
            tot_tok.to_formatted_string(&self.number_format),
        )
    }

    fn print_report_total(&mut self, report: &Report, inaccurate: bool) -> io::Result<()> {
        if report.stats.blobs.is_empty() {
            return Ok(());
        }

        let mut subtotal = tokei::Report::new("|- (Total)".into());
        subtotal.stats.code += report.stats.code;
        subtotal.stats.comments += report.stats.comments;
        subtotal.stats.blanks += report.stats.blanks;

        for (language_type, stats) in &report.stats.blobs {
            self.print_report(*language_type, stats, inaccurate)?;
            subtotal.stats += stats.summarise();
        }

        self.print_report_with_name(report)?;
        Ok(())
    }

    fn print_report_with_name(&mut self, report: &Report) -> io::Result<()> {
        let raw = report.name.to_string_lossy();
        let name = self.truncate_name_for_width(&raw, self.first_col_width);
        self.print_report_total_formatted(Cow::Owned(name), self.first_col_width, report)
    }

    fn print_report_total_formatted(
        &mut self,
        name: Cow<'_, str>,
        max_len: usize,
        report: &Report,
    ) -> io::Result<()> {
        let files_column_width: usize = files_col_width();
        let tot_tok = report.stats.source_code_tokens + report.stats.comment_tokens;

        writeln!(
            self.writer,
            "{: <max$} \
             {:>files_column_width$} \
             {:>LINES_COLUMN_WIDTH$} \
             {:>CODE_COLUMN_WIDTH$} \
             {:>COMMENTS_COLUMN_WIDTH$} \
             {:>BLANKS_COLUMN_WIDTH$} \
             {:>SOURCE_TOKENS_COLUMN_WIDTH$} \
             {:>COMMENT_TOKENS_COLUMN_WIDTH$} \
             {:>TOTAL_TOKENS_COLUMN_WIDTH$}",
            name,
            "", // no file-count here
            report
                .stats
                .lines()
                .to_formatted_string(&self.number_format),
            report.stats.code.to_formatted_string(&self.number_format),
            report
                .stats
                .comments
                .to_formatted_string(&self.number_format),
            report.stats.blanks.to_formatted_string(&self.number_format),
            report
                .stats
                .source_code_tokens
                .to_formatted_string(&self.number_format),
            report
                .stats
                .comment_tokens
                .to_formatted_string(&self.number_format),
            tot_tok.to_formatted_string(&self.number_format),
            max = max_len
        )
    }

    pub fn print_total(&mut self, languages: &tokei::Languages) -> io::Result<()> {
        let total = languages.total();
        self.print_row()?;
        self.print_language_in_print_total(&total)?;
        self.print_row()
    }
}
