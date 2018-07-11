// activate test feature when "nightly" is set
#![cfg_attr(feature = "nightly", feature(test))]
extern crate combine;
extern crate ecmascript;
extern crate serde;
#[macro_use]
extern crate serde_derive;
extern crate glob;
extern crate serde_json;
#[cfg(all(feature = "nightly", test))]
extern crate test;

#[cfg(all(feature = "nightly", test))]
mod nightly_integration_tests {
    use ecmascript;
    use glob::glob;
    use serde_json::{self, Value};
    use std::env;
    use std::fs::File;
    use test::ShouldPanic::No;
    use test::{test_main, Options, TestDesc, TestDescAndFn, TestFn, TestName};

    #[derive(Serialize, Deserialize, Debug)]
    #[serde(untagged)]
    enum TestFixture {
        Success {
            name: String,
            source: String,
            result: Value,
        },
        Failure {
            name: String,
            source: String,
            error: String,
        },
    }

    impl TestFixture {
        fn name(&self) -> String {
            match self {
                TestFixture::Success { name, .. } => name.clone(),
                TestFixture::Failure { name, .. } => name.clone(),
            }
        }

        fn source(&self) -> String {
            match self {
                TestFixture::Success { source, .. } => source.clone(),
                TestFixture::Failure { source, .. } => source.clone(),
            }
        }
    }

    fn add_test(tests: &mut Vec<TestDescAndFn>, scope: &str, test_fixture: TestFixture) {
        tests.push(TestDescAndFn {
            desc: TestDesc {
                name: TestName::DynTestName(format!(
                    "parser_integration_test::{}::{}",
                    scope,
                    test_fixture.name()
                )),
                ignore: false,
                should_panic: No,
                allow_fail: false,
            },
            testfn: TestFn::DynTestFn(Box::new(move || {
                let result = ecmascript::parse(&test_fixture.source())
                    .map(|result| serde_json::to_value(result).unwrap());
                match (result, test_fixture) {
                    (Ok(value), TestFixture::Success { result, .. }) => {
                        println!(
                            "left: {}, right: {}",
                            serde_json::to_string(&value).unwrap(),
                            serde_json::to_string(&result).unwrap()
                        );
                        assert_eq!(value, result)
                    }
                    (Ok(value), TestFixture::Failure { source, error, .. }) => {
                        println!("Expecting {:?} to fail with message {:?}", source, error);
                        println!("But it passed with {:?}", value);
                        panic!()
                    }
                    (Err(e), TestFixture::Success { source, .. }) => {
                        println!("Source: {:?}\n\nError: {}", source, e);
                        panic!()
                    }
                    (Err(e), TestFixture::Failure { error, .. }) => {
                        assert_eq!(e.to_string(), error)
                    }
                }
            })),
        });
    }

    pub fn main() {
        let args: Vec<_> = env::args().collect();
        let mut tests = Vec::new();
        for entry in glob("tests/fixtures/*.json").unwrap() {
            let path = entry.unwrap();
            let scope = path.file_stem().unwrap().to_str().unwrap();
            let file = File::open(&path).unwrap();
            let fixtures: Vec<TestFixture> = serde_json::from_reader(file).unwrap();
            for fixture in fixtures {
                add_test(&mut tests, scope, fixture);
            }
        }
        test_main(&args, tests, Options::new());
    }
}

#[cfg(all(feature = "nightly", test))]
fn main() {
    nightly_integration_tests::main()
}

#[cfg(not(all(feature = "nightly", test)))]
fn main() {
    println!("Sorry! Integration testing is not supported on the stable or beta channels yet.")
}
