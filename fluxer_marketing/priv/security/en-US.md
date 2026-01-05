If you believe you have found a security vulnerability in Fluxer, please report it responsibly. This policy explains what is in scope, how to submit a report, what we need from you, and what you can expect from us.

## Safe harbor

If you follow this policy, act in good faith, and avoid privacy violations or service disruption, Fluxer will not pursue legal action against you for your security research.

## Table of contents

- [Who should read this](#who-should-read-this)
- [Scope](#scope)
  - [In scope](#in-scope)
  - [Out of scope](#out-of-scope)
- [How to report](#how-to-report)
- [What we need from you](#what-we-need-from-you)
- [Rewards and recognition](#rewards-and-recognition)
- [What to expect from us](#what-to-expect-from-us)
- [Safe testing rules](#safe-testing-rules)

## Who should read this

Security researchers, community members, and anyone who discovers a potential security issue in Fluxer should read this policy before sending a report. It explains what is in scope, how we triage findings, and how we acknowledge and reward responsible disclosures.

## Scope

### In scope

Fluxer websites, applications, and services operated by Fluxer Platform AB, including the domains below:

| In-scope domains                                   |
| -------------------------------------------------- |
| `fluxer.gg`, `*.fluxer.gg`                         |
| `fluxer.gift`, `*.fluxer.gift`                     |
| `fluxerapp.com`, `*.fluxerapp.com`                 |
| `fluxer.dev`, `*.fluxer.dev`                       |
| `fluxerusercontent.com`, `*.fluxerusercontent.com` |
| `fluxerstatic.com`, `*.fluxerstatic.com`           |
| `fluxer.media`, `*.fluxer.media`                   |
| `fluxer.app`, `*.fluxer.app`                       |

Also in scope:

- Infrastructure, systems, and operational services directly managed by Fluxer that impact authentication, authorization, payments, community data, or the processing of security- or privacy-relevant data (including user identifiers, account metadata, logs, analytics, telemetry, and similar signals).
- Abuse cases that enable unauthorized persistence, privilege escalation, or data disclosure when triggered through officially supported product features.
- Self-hosted Fluxer instances that declare trust in Fluxer security guidance, provided:
  - the issue is reproducible on the latest official release as we ship it, and
  - the issue is not solely caused by third-party modifications or local misconfiguration.

If you are unsure whether a target is in scope, email us and ask.

### Out of scope

The following are out of scope (not an exhaustive list):

- Third-party services, infrastructure, or integrations we do not control (for example partner communities' independent integrations, bots, or external hosting providers).
- Vulnerabilities that require physical access to facilities, servers, or devices.
- Social engineering, phishing, bribery, coercion, or attempts to manipulate Fluxer staff or users.
- Denial-of-service (DoS) attacks, traffic flooding, rate-limit exhaustion, or resource exhaustion testing.
- Automated scanning or bulk testing that produces noisy/low-signal findings, especially without a clear security impact and a reliable reproduction path.
- General UI bugs, feature requests, or non-security support issues (email support@fluxer.app for those).
- Issues in forked, modified, or outdated self-hosted deployments that are not reproducible on the latest official release.

In addition, we generally do not prioritize low-impact reports (for example missing best-practice headers or minor configuration issues) unless you can demonstrate a concrete security impact.

## How to report

Email your report to security@fluxer.ap.

Please include:

- A short, descriptive title.
- Why the issue is a security concern (impact, affected users/systems, and realistic attack scenario).
- Step-by-step reproduction instructions and any proof of concept (screenshots, logs, recordings, or curl commands are helpful).

Please do not publicly disclose the vulnerability until we have acknowledged your report and had a reasonable opportunity to investigate and ship a fix. We may coordinate a disclosure timeline with you.

## What we need from you

To help us validate and fix the issue quickly, include as much of the following as you can:

- A clear summary of the issue and its impact.
- Step-by-step reproduction instructions.
- The environment you used (browser, operating system, client version, region, logged-in state, etc.).
- Any mitigations you tried, and whether the issue persists after clearing caches, using a private window, or restarting clients.
- A severity estimate (for example a CVSS score), or a plain-language assessment of the access/impact the issue enables.

## Rewards and recognition

Depending on validity, severity, and impact, we may award:

- A Bug Hunter badge on your Fluxer profile.
- Plutonium gift codes on fluxer.app so you can access premium features.

Higher-severity findings receive more recognition. We intend to add cash payouts in the future once our payments tooling is ready. At this time, we do not guarantee monetary rewards, but we do credit valid research that follows this policy.

### Credit and eligibility

- Please report findings privately to security@fluxer.app.
- Public disclosure before we acknowledge and address the issue may make the report ineligible for rewards or recognition.
- If multiple reports describe the same underlying issue, we typically credit the first report that clearly explains the vulnerability and enables reliable reproduction.

## What to expect from us

- **Acknowledgement:** We aim to acknowledge reports within five business days (often sooner).
- **Triage and updates:** We will review your report, prioritize critical issues, and keep you updated as we investigate.
- **Resolution and disclosure:** After a fix is available, we typically coordinate disclosure and credit with the reporter, unless you prefer to remain anonymous.
- **If we cannot reproduce:** We will share what we tried and may ask for additional details, environment information, or a clearer proof of concept.

## Safe testing rules

- Only test against accounts, communities, and data you own or have explicit permission to use.
- Community-level testing (roles, permissions, invites, moderation tools, settings, data access, etc.) must be performed only in communities you own/admin, or where you have explicit permission from the community owner/admin.
- Do not access, modify, or attempt to view other users' or other communities' data without consent.
- Do not use automated flooding, scraping, brute forcing, or other disruptive techniques.
- Do not use scanners or automated tools in ways that degrade reliability or create noisy/low-signal reports.
- If your testing could trigger real user notifications, support workflows, emails, billing events, or payments, contact us first so we can monitor.
- Follow applicable laws where you live and where the systems operate. If you are unsure, err on the side of caution and ask before escalating a high-impact test.

Thank you for helping keep Fluxer secure.
