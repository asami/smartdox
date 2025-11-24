# Contributing to This Project

Thank you for your interest in contributing to SmartDox / Cozy and the  
SimpleModeling ecosystem!  
We welcome contributions of all kinds — code, documentation, examples,  
specifications, and ideas.

This guide explains the contribution process, coding standards, and legal  
requirements for contributing to the project.

---

## 1. Code of Conduct

This project follows a respectful and open community philosophy:  
- Be kind and constructive  
- Assume good intent  
- Help others grow  
- Keep technical discussions productive  

A formal Code of Conduct may be introduced later.

---

## 2. Contributor License Agreement (CLA)

Before your contribution can be merged, you must agree to the:

**Contributor License Agreement (CLA)**  
`CLA.md`

By submitting a pull request, you indicate your acceptance of the CLA.

This allows your contribution to be included in both:

- the open-source distribution (Apache 2.0 / CC-BY-SA), and  
- future commercial distributions, if any.

---

## 3. Project Structure

This repository contains both software and documentation assets.

### Software (Code)
Licensed under **Apache License 2.0**.  
Relevant directories:
```
src/
project/
build.sbt  (or package.json)
```

### Documentation (SmartDox / text content)
Licensed under **CC-BY-SA 4.0**.  
Relevant directories:
```
docs/
examples/
spec/
```

See `LICENSE` and `DOC_LICENSE.md` for details.

---

## 4. How to Contribute

### ✔ 4.1. Reporting Issues

Issues are welcome!  
When reporting a problem, please include:

- Expected behavior  
- Actual behavior  
- Steps to reproduce  
- Environment details (OS, JVM/Scala version, etc.)  
- Minimal example if possible

### ✔ 4.2. Requesting Features

Feature requests are also welcome.  
Please describe:

- Your use case  
- Why it is important  
- Expected behavior  
- Any alternatives considered

### ✔ 4.3. Submitting Pull Requests (PRs)

1. Fork the repository  
2. Create a feature branch  
3. Make your changes  
4. Add tests (if applicable)  
5. Ensure all tests pass  
6. Submit a Pull Request with a clear description

PR Guidelines:
- Keep changes focused  
- Explain why the change is needed  
- Reference related issues  
- Follow the coding style  
- Sign contributions by submitting the PR (which implies CLA agreement)

---

## 5. Coding Guidelines

### Scala (SmartDox / Cozy)
- Prefer functional style (pure functions, immutability)
- Use clear and explicit types
- Avoid unnecessary complexity
- Follow project formatting (`scalafmt` if configured)
- Provide unit tests when adding new behavior

### Documentation (SmartDox)
- Use SmartDox structured sections (HEAD / SUMMARY / LEAD / REMARKS)
- Prefer bilingual (EN/JA) where applicable
- Place documents under `docs/` or `examples/`
- Use consistent terminology in the SimpleModeling ecosystem
- Keep examples small but meaningful

---

## 6. Adding Documentation

Documentation contributions are highly valued.

You may contribute:

- SmartDox example files  
- Specs / RFC-style documents  
- Tutorials and QuickStart guides  
- Knowledge-modeling examples  
- Diagrams (UML, PlantUML, Semantic Message Flow diagrams)

Documentation is licensed under CC-BY-SA 4.0.

---

## 7. Tests

If your change affects code behavior, please add tests.

Test guidelines:
- Keep tests deterministic  
- Prefer small, unit-level tests  
- Add example-based tests for SmartDox transformations  
- Document expected behaviors in comments where helpful

---

## 8. Build Instructions

### Scala Projects
```
sbt compile
sbt test
sbt run
```

### JavaScript/TypeScript (if Cozy uses JS output)
```
npm install
npm test
npm run build
```

---

## 9. Communication

If you wish to discuss design ideas or larger contributions, you can reach out:

**ASAMI, Tomoharu / SimpleModeling.org**  
Maintained by **Asami Office Inc. ((有)浅海智晴事務所)**

Website: https://www.simplemodeling.org  
Email: info@simplemodeling.org

---

## 10. Thank You

Your contributions help grow the SimpleModeling ecosystem and strengthen  
the future of literate modeling, structured documentation, and AI-assisted  
software engineering.

We deeply appreciate your support.
