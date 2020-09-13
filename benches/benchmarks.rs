use criterion::{criterion_group, criterion_main, Criterion};
use trashy_xml::{tokens::XmlKind, XmlParser};

fn small_file_attributes() -> usize {
    // quick-xml was at 30us
    let mut result = 0;
    let mut parser = XmlParser::new("sample_files/small.xml").unwrap();
    parser.parse();
    for token in &parser.xml_tokens {
        if let XmlKind::OpenElement(name, _) = &token.kind {
            if parser.str(name) == "var_compond" {
                for _ in parser.attributes(token) {
                    result += 1;
                }
            }
        }
    }
    result
}

fn large_file_token_length() -> usize {
    // quick-xml was at 9ms
    let mut parser = XmlParser::new("sample_files/large.xml").unwrap();
    parser.parse();
    parser.xml_tokens.len()
}

fn medium_file_attributes() -> usize {
    // quick-xml was at 30us
    let mut result = 0;
    let mut parser = XmlParser::new("sample_files/medium.xml").unwrap();
    parser.parse();
    for token in &parser.xml_tokens {
        if let XmlKind::OpenElement(name, _) = &token.kind {
            if parser.str(name) == "Text" {
                for _ in parser.attributes(token) {
                    result += 1;
                }
            }
        }
    }
    result
}

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("small_file_attributes", |b| b.iter(small_file_attributes));
    c.bench_function("medium_file_attributes", |b| b.iter(medium_file_attributes));
    c.bench_function("large_file_token_length", |b| {
        b.iter(large_file_token_length)
    });
}

criterion_group!(
    name = benches;
    config = Criterion::default().sample_size(20);
    targets = criterion_benchmark
);
criterion_main!(benches);
