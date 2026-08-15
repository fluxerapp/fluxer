# LLM Usage Policy

## Abstract

This document defines how a person without write access to this repository may use a large language model (LLM) when preparing a contribution.

An LLM may assist a contributor privately with learning, investigation, planning and review. It MUST NOT author any part of a submission.

## 1. Introduction

The purpose of this policy is to ensure that every submission is the work of the person who submits it. Contributors may use an LLM as a private learning and analysis tool, subject to the requirements below, but they remain the sole authors of their submissions.

## 2. Requirements Language

The key words "MUST", "MUST NOT" and "MAY" in this document are to be interpreted as described in BCP 14 [RFC2119] [RFC8174] when, and only when, they appear in all capitals, as shown here.

A contributor complies with this policy only if the contributor satisfies every applicable MUST and MUST NOT requirement.

## 3. Scope

This policy applies to every person who does not have write access to this repository.

It applies to all content that such a person provides to the maintainers or publishes through the repository, including:

- Issues and discussions.
- Security reports.
- Commits and pull requests.
- Review comments and replies.
- Documentation and source comments.
- Release notes and other repository text.
- Images, audio and video.

This policy applies regardless of how an LLM is accessed. Covered interfaces include chatbots, coding assistants, autonomous or semi-autonomous agents, and model-powered editor completion.

## 4. Definitions

For the purposes of this policy:

- **LLM** means a large language model or any other generative model that produces code, prose, images, audio or video.
- **Contributor** means a person who is subject to this policy.
- **Submission** means any content that a contributor provides to the maintainers or publishes through the repository.
- **LLM output** means any code, prose, image, audio or video produced by an LLM.
- **LLM-generated content** means LLM output and any content derived from it. Content remains LLM-generated after it has been edited, corrected, rewritten, paraphrased, translated, reformatted or combined with other material.
- **Independent work** means content created by the contributor without copying, paraphrasing, translating, adapting or completing LLM output.

Section 6 provides the only exception to the prohibition on submitting LLM-generated text.

## 5. Permitted Private Use

A contributor MAY use an LLM privately to:

- Research external material.
- Inspect and understand the repository.
- Ask questions about existing code or documentation.
- Explore possible approaches to a problem.
- Plan an implementation.
- Interpret compiler output, test failures, logs or other diagnostic information.
- Review code or prose that the contributor wrote independently.

The contributor MUST keep the LLM output private. The contributor MUST NOT publish it, include it in a submission or require another person to read or evaluate it.

If an LLM suggests code, wording, a defect or a solution, the contributor MUST verify the underlying information independently. The contributor MUST then produce any resulting submission as independent work, using the contributor's own understanding and judgement. The contributor MUST NOT copy, paraphrase, translate, adapt or otherwise reproduce the suggestion.

## 6. Machine Translation

A contributor MAY use machine translation only to translate text that the contributor wrote independently.

When submitting a machine translation, the contributor:

- MUST disclose that machine translation was used;
- MUST verify that the translation has not added, removed or altered any claim; and
- MUST include the original text with the translation so that readers can resolve any ambiguity.

Machine translation is a limited exception to the prohibition on submitting LLM-generated text. It MUST NOT be used to draft, rewrite, expand, summarise or improve the original text.

## 7. Excluded Tools

This policy does not apply to deterministic formatters, linters, codemods, compilers or repository-owned code generators.

It also does not apply to ordinary non-generative editor features, including identifier completion, bracket completion and fixed text snippets.

Model-powered completion is LLM use and remains subject to this policy.

## 8. Prohibited Use

A contributor MUST NOT submit code, prose or media that an LLM has written, rewritten, expanded or completed.

In particular, a contributor MUST NOT use an LLM to author any submitted:

- Code or tests.
- Documentation or source comments.
- Commit messages.
- Pull request titles or descriptions.
- Issues, discussions or security reports.
- Review comments or replies.
- Release notes or other repository text.

A contributor MUST NOT submit an LLM-generated image, audio recording or video.

A contributor MUST NOT direct or permit an autonomous or semi-autonomous agent to create, edit, open, submit or comment on an issue, discussion, security report or pull request.

Disclosure of LLM use does not make prohibited content acceptable.

## 9. Authorship and Responsibility

A contributor MUST understand every submitted line and MUST be able to explain:

- What it does or means.
- Why it is necessary.
- Why it is correct.

The contributor remains fully responsible for the submission. An LLM suggestion or error does not excuse an inaccurate claim, defective code, security vulnerability, licensing violation or other harm.

An LLM review does not replace the contributor's own review or a maintainer's review. A person exercising independent judgement MUST make every decision that affects a contributor or the repository.

## 10. Review and Enforcement

A maintainer MAY ask a contributor to explain any part of a submission. A maintainer MAY close a submission if the contributor cannot explain it adequately.

Maintainers MUST close a submission that contains prohibited content. A maintainer MAY permit a new submission only if it was written independently and complies with this policy.

Any of the following MAY result in the contributor being blocked from the repository:

- Deliberately concealing LLM use.
- Using an autonomous or semi-autonomous agent to submit content.
- Repeatedly violating this policy.

Deliberately submitting a fabricated security report MAY result in an immediate block. A security report is fabricated only if the contributor knowingly invented or falsified a material claim. A report that is incorrect but was submitted in good faith is not fabricated.

Maintainers are not required to investigate possible LLM use proactively. Writing style alone is not evidence of a violation.

A person MUST NOT publicly accuse or harass a contributor because of suspected LLM use. All discussion, review and enforcement under this policy MUST comply with the [Code of Conduct](CODE_OF_CONDUCT.md).

## 11. Normative References

- **[RFC2119]** S. Bradner, [_Key words for use in RFCs to Indicate Requirement Levels_](https://www.rfc-editor.org/info/rfc2119), BCP 14, RFC 2119, March 1997.
- **[RFC8174]** B. Leiba, [_Ambiguity of Uppercase vs Lowercase in RFC 2119 Key Words_](https://www.rfc-editor.org/info/rfc8174), BCP 14, RFC 8174, May 2017.
