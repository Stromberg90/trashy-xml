use criterion::{criterion_group, criterion_main, Criterion};
use trashy_xml::XmlParser;

fn small_file_attributes() -> usize {
    // quick-xml was at 30us
    let mut result = 0;
    let parsed = XmlParser::file("sample_files/small.xml").unwrap().parse();
    for token in parsed.elements_from_name("var_compond") {
        for _ in token.attributes() {
            result += 1;
        }
    }
    result
}

fn medium_file_attributes() -> usize {
    // quick-xml was at 30us
    let mut result = 0;
    let parsed = XmlParser::file("sample_files/medium.xml").unwrap().parse();
    for token in parsed.elements_from_name("Text") {
        for _ in token.attributes() {
            result += 1;
        }
    }
    result
}

fn large_file_token_length() -> usize {
    // quick-xml was at 9ms
    let parsed = XmlParser::file("sample_files/large.xml").unwrap().parse();
    parsed.elements().count()
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
