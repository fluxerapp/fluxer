---
# SPDX-License-Identifier: AGPL-3.0-or-later
title: Contributing
description: Who can open a pull request to Fluxer, how to use AI tools, and what every commit and pull request needs.
---

Fluxer's code is on [GitHub](https://github.com/fluxerapp/fluxer). These rules apply when you contribute to Fluxer.

## Before you start

Only contributors a maintainer has approved can open pull requests, and each pull request needs a maintainer's go-ahead on its feedback post first. To get it, follow these steps.

1. Find the post on [feedback.fluxer.com](https://feedback.fluxer.com) that your change resolves. If there is none, post a bug report or, for anything bigger than a bug fix, a feature request there first.
2. Comment on the post and ask to work on it.
3. Once a maintainer says you can work on it, open your pull request.

Every pull request targets the default branch, links each post it resolves on its own line, and needs a maintainer's approval before it merges.

```text
Resolves https://feedback.fluxer.com/p/123
Resolves https://feedback.fluxer.com/p/456
```

Keep each pull request to one change, and put any unrelated fixes, refactoring or formatting in a separate pull request.

[Set up a development environment](/development-environment/) to run Fluxer from source and run its checks.

## AI tools

You write everything you submit yourself. That includes code, tests, comments, commit messages, pull request text, security reports and your replies to code reviews.

How you use AI tools privately is up to you. Asking an AI agent where something is in a large codebase or how a part of it works is a good idea, especially when you're new to it.

<figure class="flx-diagram flx-diagram--start">
<svg class="flx-diagram-wide" viewBox="0 0 560 300" role="img" aria-labelledby="flx-ai-venn-title flx-ai-venn-desc" fill="none" stroke="currentColor" font-family="inherit" font-size="13">
	<title id="flx-ai-venn-title">Learning a codebase with an AI and vibe coding don't overlap</title>
	<desc id="flx-ai-venn-desc">Two circles that don't touch. In one, you learn with an AI. You ask where code is and how a part works, then write every line yourself and can explain all of it. In the other, vibe coding, an AI writes the code, it ships unread, and nobody understands or can explain it. The circles have nothing in common.</desc>
	<circle cx="130" cy="150" r="128" stroke-opacity="0.6" />
	<text x="130" y="106" fill="currentColor" stroke="none" text-anchor="middle" font-weight="600" font-size="15">Learning with an AI</text>
	<text x="130" y="134" fill="currentColor" stroke="none" text-anchor="middle">Ask where code is</text>
	<text x="130" y="156" fill="currentColor" stroke="none" text-anchor="middle">Ask how a part works</text>
	<text x="130" y="178" fill="currentColor" stroke="none" text-anchor="middle">You write every line</text>
	<text x="130" y="200" fill="currentColor" stroke="none" text-anchor="middle">You can explain all of it</text>
	<circle cx="430" cy="150" r="128" stroke-opacity="0.6" stroke-dasharray="5 5" />
	<text x="430" y="106" fill="currentColor" stroke="none" text-anchor="middle" font-weight="600" font-size="15">Vibe coding</text>
	<text x="430" y="134" fill="currentColor" stroke="none" text-anchor="middle">An AI writes the code</text>
	<text x="430" y="156" fill="currentColor" stroke="none" text-anchor="middle">It ships unread</text>
	<text x="430" y="178" fill="currentColor" stroke="none" text-anchor="middle">Nobody understands it</text>
	<text x="430" y="200" fill="currentColor" stroke="none" text-anchor="middle">Nobody can explain it</text>
</svg>
<svg class="flx-diagram-narrow" viewBox="0 0 276 600" role="img" aria-labelledby="flx-ai-venn-title-narrow flx-ai-venn-desc-narrow" fill="none" stroke="currentColor" font-family="inherit" font-size="13">
	<title id="flx-ai-venn-title-narrow">Learning a codebase with an AI and vibe coding don't overlap</title>
	<desc id="flx-ai-venn-desc-narrow">Two circles that don't touch. In one, you learn with an AI. You ask where code is and how a part works, then write every line yourself and can explain all of it. In the other, vibe coding, an AI writes the code, it ships unread, and nobody understands or can explain it. The circles have nothing in common.</desc>
	<circle cx="138" cy="152" r="136" stroke-opacity="0.6" />
	<text x="138" y="108" fill="currentColor" stroke="none" text-anchor="middle" font-weight="600" font-size="15">Learning with an AI</text>
	<text x="138" y="136" fill="currentColor" stroke="none" text-anchor="middle">Ask where code is</text>
	<text x="138" y="158" fill="currentColor" stroke="none" text-anchor="middle">Ask how a part works</text>
	<text x="138" y="180" fill="currentColor" stroke="none" text-anchor="middle">You write every line</text>
	<text x="138" y="202" fill="currentColor" stroke="none" text-anchor="middle">You can explain all of it</text>
	<circle cx="138" cy="448" r="136" stroke-opacity="0.6" stroke-dasharray="5 5" />
	<text x="138" y="404" fill="currentColor" stroke="none" text-anchor="middle" font-weight="600" font-size="15">Vibe coding</text>
	<text x="138" y="432" fill="currentColor" stroke="none" text-anchor="middle">An AI writes the code</text>
	<text x="138" y="454" fill="currentColor" stroke="none" text-anchor="middle">It ships unread</text>
	<text x="138" y="476" fill="currentColor" stroke="none" text-anchor="middle">Nobody understands it</text>
	<text x="138" y="498" fill="currentColor" stroke="none" text-anchor="middle">Nobody can explain it</text>
</svg>
<figcaption>Learning a codebase with an AI and vibe coding have nothing in common.</figcaption>
</figure>

An AI agent must never write the code, tests or text you submit. Never let an AI agent open, edit or comment on pull requests for you, even when you check everything it does.

Machine translation is fine for pull request descriptions and review comments you wrote yourself. Say that you used it, and include your original text with the translation.

You have to understand every line you submit and explain what it does and why it's right. A maintainer can ask, and can close a pull request you can't explain. If you break these AI rules or send a made-up security report, a maintainer can block you from the repository.

## Commits

Every commit needs:

- A [Developer Certificate of Origin](https://developercertificate.org) sign-off. Commit with `git commit -s`, and use the same name and email as the commit author.
- A signature that GitHub shows as verified.

The sign-off confirms that you wrote the change, or have the right to submit it, under the [AGPL-3.0-or-later licence](https://github.com/fluxerapp/fluxer/blob/main/LICENSE), which covers all of Fluxer's code.

## Pull requests

Pull request titles use this format, and the subject after the colon is 72 characters or fewer.

```text
type(optional-scope): imperative subject
```

The type is one of `feat`, `fix`, `docs`, `style`, `refactor`, `perf`, `test`, `build`, `ci` or `chore`. Add `!` before the colon for a breaking change. For a revert, put `revert: ` in front of the original title, as in `revert: fix(api): handle empty request bodies`.

Fill in every section of the pull request template. Say what changes, why it's correct, what could break and how you checked it.

## Build version and channel

Two environment variables set the version and channel of a desktop or web build. A local build works fine with both of them unset.

| Variable | What it sets | When unset |
| --- | --- | --- |
| `BUILD_VERSION` | The CalVer version (`YYYY.MDD.MICRO`) that the web app shows in its build info and `version.json`, and that the desktop app uses as its own version | `dev` for the web app. A desktop package takes the version of the web app it bundles. |
| `BUILD_CHANNEL` | `stable`, `canary` or `development`. It sets the desktop app's ID, data folder, link protocol and where it checks for updates, and the web app's channel when the page has no channel tag. | `canary` for the web app and `stable` for the desktop app |

The desktop release workflows set both, and the web app release sets only `BUILD_VERSION`. The app-proxy has its own `RELEASE_CHANNEL` setting, and when it is `stable` or `canary` the app-proxy adds a `fluxer-release-channel` meta tag with that value each time it serves the page. Otherwise a self-hosted app-proxy tags the page `stable`, and any other app-proxy adds no tag.

## Other ways to help

- Report bugs and request features at [feedback.fluxer.com](https://feedback.fluxer.com).
- Report security problems privately through [fluxer.com/security](https://fluxer.com/security), never in public.
- Translate Fluxer on [Weblate](https://weblate.fluxer.tools). Pull requests that change translations aren't accepted.
- [Fund Fluxer's open source work](https://fluxer.com/help/fund-fluxer) with a donation or, for organisations, a grant.

The [code of conduct](/code-of-conduct/) applies to everything you do in the Fluxer repository, and feedback.fluxer.com has its own [code of conduct](https://feedback.fluxer.com/conduct).
