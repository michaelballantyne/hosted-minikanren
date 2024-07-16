# Hosted miniKanren

Welcome to the Hosted miniKanren repository! This project is a compiler-based implementation of the miniKanren language, partly described in our [ICFP paper][link-to-paper]. This README provides an overview of the language implementation, documentation, resources, and instructions on how to build, run, and test the project.

## Table of Contents

- [Introduction](#introduction)
- [Documentation](#documentation)
- [Resources](#resources)
- [Artifact from ICFP Paper](#artifact-from-icfp-paper)
- [Building the Project](#building-the-project)
- [Running the Project](#running-the-project)
- [Testing the Project](#testing-the-project)
- [Contributing](#contributing)
- [License](#license)

## Introduction

miniKanren is a family of domain-specific languages for logic programming. This repository contains the `hosted-minikanren` implementation, which utilizes a compiler-based approach for improved performance and flexibility. The primary goals of this implementation are to:

- Provide a robust and efficient platform for logic programming.
- Enable users to experiment with and extend the miniKanren language.
- Serve as a reference implementation for research and educational purposes.

## Documentation

Detailed documentation for Hosted miniKanren can be found in the `docs` directory. This includes:

- [User Guide](docs/user_guide.md): Comprehensive instructions on using Hosted miniKanren.
- [Developer Guide](docs/developer_guide.md): Information on the internal architecture and how to contribute to the project.

## Resources

Here are some useful resources related to miniKanren and logic programming generally:

- [miniKanren Website](http://minikanren.org): Official website with tutorials, papers, and community links.
- [ICFP Paper][link-to-paper]: The paper describing this implementation.
- [miniKanren GitHub Organization](https://github.com/miniKanren): Other projects and implementations related to miniKanren.

## Code from ICFP 2024 Paper

The code corresponding to our ICFP 2024 paper is available in the [`demos/icfp2024` sub-directory](demos/icfp2024).

## Building the Project

To build `hosted-minikanren`,

1. Ensure you have Racket installed and `racket` and `raco`  available on PATH.

2. Clone the repository:
   ```sh
   git clone https://github.com/michaelballantyne/hosted-minikanren.git
   cd hosted-minikanren
   ```

3. Setup:
   ```sh
   raco setup hosted-minikanren
   ```

## Running the Project

With this implementation, running a miniKanren program is as easy as running another Racket program. Save a file like `test-mk.rkt`

```racket
#lang racket
(require hosted-minikanren)

(defrel (cato x)
  (== x 'cat))

(run 1 (q) (cato q))
```

For more examples and usage instructions, refer to the [User Guide](docs/user_guide.md).

## Testing the Project

Beyond the tests that come with `faster-minikanren`, we also use an additional compiler-specific suite of tests. To run both sets of tests, execute:

```sh
   raco test -c hosted-minikanren
```

You can also run specific tests or test categories as described in the [Developer Guide](docs/developer_guide.md).

For the code from the ICFP paper, see the documentation in the [artifact associated with the paper](./demos/icfp2024/README.md).

To run our benchmark tests see [the associated documentation](./bench/README.md)

## Contributing

We welcome contributions from the community! PRs welcome!

## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for more details.


[link-to-paper]: https://mballantyne.net/publications/icfp2024.pdf
