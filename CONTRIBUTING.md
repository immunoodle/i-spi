# Contributing to The Interactive Serology Plate Inspector (I-SPI)

Thank you for your interest in contributing to **I-SPI**! We welcome contributions from the community to help improve the project. This document outlines how you can help and the best practices to follow when contributing.

---

## Table of Contents

- [How Can I Contribute?](#how-can-i-contribute)
- [Code Style](#code-style)
- [Testing](#testing)
- [Submitting Pull Requests](#submitting-pull-requests)
- [Code of Conduct](#code-of-conduct)
- [Additional Resources](#additional-resources)

---

## How Can I Contribute?

You can contribute in several ways:

- **Reporting bugs or suggesting features** via GitHub Issues.
- **Improving documentation** to make it clearer and more comprehensive.
- **Submitting code contributions** (features, bug fixes, refactorings).
- **Reviewing pull requests** and providing constructive feedback.

---

## Code Style

Maintaining a consistent code style helps everyone read and maintain the codebase seamlessly.

- Follow the existing code style of the project.
- Adhere to [language-specific style guides], for example:
  - For R, use [Advanced R by Hadley Wickham](http://adv-r.had.co.nz/Style.html)
  - For JavaScript/TypeScript, use [Airbnb Style Guide](https://github.com/airbnb/javascript) or project-specific ESLint rules.
  - For Python, use [PEP8](https://www.python.org/dev/peps/pep-0008/).
- Use meaningful variable and function names.
- Write clear, concise, and well-documented code.
- Add comments where the code’s purpose or logic is not immediately obvious.
- Format your code using the project's formatter (e.g. Prettier, Black) if applicable.

---

## Testing

Ensuring your changes work as expected and that you don’t break existing functionality is crucial.

- Write tests for new features and bug fixes.
- Tests should be deterministic and repeatable.
- Use the testing framework already in place (e.g., Jest, Mocha, PyTest).
- Run all existing tests locally before submitting.
- Include clear descriptions of what your tests cover.
- Cover edge cases and error handling where relevant.
- For documentation improvements, preview the generated docs if possible.

### Submitting Pull Requests
Thank you for contributing! To make the review process smooth and efficient, please follow these guidelines:
Fork the repository and create your branch from main (or the default branch):
    - git checkout -b feature/your-feature-name
Make your changes in a single logical commit or multiple related commits.
-  Write clear commit messages:
-  Use the imperative mood (e.g., "Fix typo in README").
-  Reference relevant issues if applicable (e.g., "Fixes #123").
-  Ensure your code passes all tests.
Update documentation if your changes affect the public API or usage.
Push your branch to your fork:
  - git push origin feature/your-feature-name
Open a Pull Request against the main repository’s main branch.
-  Provide a clear description of what you've changed and why.
-  Link any related issues.
-  Explain any special considerations or deployment notes.
-  Respond to feedback and update your pull request as needed.
-  
### Contributor Code of Conduct

## Our Pledge
We are committed to fostering an open and welcoming environment where everyone feels safe to contribute. All participants are expected to uphold this spirit through respectful, kind, and inclusive behavior.

## Our Standards
Examples of behavior that contributes to creating a positive environment include:

- Using welcoming and inclusive language
- Being respectful of differing viewpoints and experiences
- Gracefully accepting constructive criticism
- Focusing on what is best for the community
- Showing empathy towards other community members

Examples of unacceptable behavior by participants include:

- Harassment, intimidation, or discrimination in any form
- Violence or threats of violence
- Offensive or derogatory comments related to gender, sexual orientation, race, ethnicity, religion, disability, or any other personal characteristic
- Public or private harassment
- Publishing others’ private information, such as physical or electronic addresses, without explicit permission

## Enforcement Responsibilities

Project maintainers are responsible for clarifying standards and enforcing this code of conduct. Maintainers have the right and responsibility to remove, edit, or reject comments, commits, code, wiki edits, issues, and other contributions that are not aligned with this Code of Conduct or to temporarily or permanently ban any contributor for other behaviors that they deem inappropriate, threatening, offensive, or harmful.

## Reporting

If you are subject to or witness unacceptable behavior, please report it as soon as possible by contacting the project maintainers at [Michael.S.Zens@dartmouth.edu].

All reports will be handled with discretion and confidentiality.

## Scope

This Code of Conduct applies both within project spaces and in public spaces when an individual is representing the project or its community. Examples include when an individual is using an official project e-mail address, posting via an official social media account, or acting as an appointed representative at an online or offline event.

## Enforcement

Instances of abusive, harassing, or otherwise unacceptable behavior may be reported by contacting the project team at [Michael.S.Zens@dartmouth.edu]. All complaints will be reviewed and investigated promptly and fairly.

Project maintainers who do not follow or enforce the Code of Conduct may face temporary or permanent consequences as determined by other members of the project’s leadership.

## Attribution

This Code of Conduct is adapted from the [Contributor Covenant, version 2.1](https://www.contributor-covenant.org/version/2/1/code_of_conduct.html), available under the [Creative Commons Attribution 4.0 International License](https://creativecommons.org/licenses/by/4.0/).

---

*Thank you for helping improve I-SPI! Your contributions make this project better for everyone.*
— The I-SPI Maintainers
