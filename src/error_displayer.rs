use std::fmt::{Debug, Display, Formatter};

use lady_deirdre::{
    format::{AnnotationPriority, SnippetConfig, SnippetFormatter},
    lexis::{PositionSpan, SourceCode},
};

pub struct DisplayError<'a, C: SourceCode> {
    pub source: &'a C,
    pub message: String,
    pub annotations: Vec<(PositionSpan, AnnotationPriority, String)>,
}

impl<'a, C: SourceCode> Debug for DisplayError<'a, C> {
    #[inline(always)]
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self, formatter)
    }
}

impl<'a, C: SourceCode> Display for DisplayError<'a, C> {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        let mut snippet = formatter.snippet(self.source);

        snippet.set_config(&const { SnippetConfig::verbose() });

        for (span, priority, text) in self.annotations.iter().rev() {
            snippet.annotate(span, *priority, text);
        }

        if !self.message.is_empty() {
            snippet.set_summary(&self.message);
        }

        snippet.finish()
    }
}
