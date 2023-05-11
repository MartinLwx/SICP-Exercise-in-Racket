from pathlib import Path


def gen_motivation() -> list[str]:
    return [
        """## Motivation
After taking CS61 course, I start to apperciate the beauty of Scheme, and I became eager to explore its capabilities further. So I begin to read the famous SCIP book and do exercise.

I use [DrRacket](https://docs.racket-lang.org/drracket/index.html) as the IDE to test my code. The explanation will be included in the comment if available."""
    ]


def get_chapter_title(idx: int) -> str:
    match idx:
        case 1:
            return "### Chapter 1. Building Abstractions with Procedures"
        case 2:
            return "### Chapter 2. Building Abstractions with Data"
        case _:
            raise ValueError(f"Not implement for Chapter{idx}")


def traverse_folder(f: Path) -> list[str]:
    checkboxs = []
    files = sorted(f.iterdir(), key=lambda x: int(x.stem))
    assert len(files) > 0, f"Expect at least one file in {f}"
    max_index = int(files[-1].stem)
    print(f"Find the maximum index in {f.stem}: {max_index}")
    for i in range(1, max_index + 1):
        i_rkt = f / f"{i:02}.rkt"
        if i_rkt.exists():
            checkboxs.append(f"- [x] [Exercise 1.{i}](./{f.stem}/{i:02}.rkt)")
        else:
            checkboxs.append(f"- [ ] [Exercise 1.{i}](./{f.stem}/{i:02}.rkt)")

    return checkboxs


def gen_exercises():
    rows = ["## Exercises"]
    for folder in Path().cwd().iterdir():
        if folder.is_dir() and folder.stem.startswith("Chapter"):
            print(f"Processing {folder}")
            rows.append(get_chapter_title(int(folder.stem.removeprefix("Chapter"))))
            rows.extend(traverse_folder(folder))
    return rows


def main():
    motivation = gen_motivation()
    exercise = gen_exercises()

    print("Generating README.md")
    rows = motivation + exercise
    with open("./README.md", "w") as f:
        f.write("\n".join(rows))
    print("Finish")


if __name__ == "__main__":
    main()
