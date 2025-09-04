use im::HashMap;

#[derive(Clone, Hash, Eq, PartialEq)]
struct Input<'a> {
    word: &'a str,
    possible: im::HashSet<&'a str>,
}

fn main() {
    // let raw_dict: String = std::fs::read_to_string("src/originalDict.txt")
    //     .expect("Could not read src/originalDict.txt");
    let raw_dict = "CUTE MUTE MULE";
    let words: im::HashSet<&str> = raw_dict.split_whitespace().collect();

    let best = best_for(HashMap::new(), words.clone(), words);

    println!("Best: {best}");
}

fn best_for<'a>(
    budget: u32,
    mut scores: HashMap<Input<'a>, f64>,
    words: im::HashSet<&'a str>,
    possible: im::HashSet<&'a str>,
) -> &'a str {
    let mut best: Option<(&'a str, f64)> = None;

    for word in words.iter() {
        let score: f64 = score_for(
            budget,
            &mut scores,
            words.clone(),
            Input {
                word,
                possible: possible.clone(),
            },
        );
        match best {
            None => best = Some((word, score)),
            Some((_, previous_best)) => {
                if score < previous_best {
                    best = Some((word, score))
                }
            }
        }
    }

    return best.expect("Empty words").0;
}

fn score_for<'a>(
    budget: u32,
    scores: &mut HashMap<Input<'a>, f64>,
    words: im::HashSet<&'a str>,
    input: Input<'a>,
) -> f64 {
    if budget == 0 {
        return 1_000_000.0;
    }

    if let Some(score) = scores.get(&input) {
        return *score;
    }

    for secret in words {
        let answer = answer_for(secret, input.word);
        let still_possible = input
            .possible
            .clone()
            .retain(|w| is_compatible_with(answer, w));
    }
    let score = 0f64;

    scores.insert(input, score);
    return score;
}
