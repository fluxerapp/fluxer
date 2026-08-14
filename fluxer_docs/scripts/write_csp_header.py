# SPDX-License-Identifier: AGPL-3.0-or-later

from __future__ import annotations

import base64
import hashlib
import html.parser
import pathlib
import sys


class InlineScriptParser(html.parser.HTMLParser):
    def __init__(self) -> None:
        super().__init__(convert_charrefs=False)
        self._collecting = False
        self._parts: list[str] = []
        self.scripts: list[str] = []

    def handle_starttag(self, tag: str, attrs: list[tuple[str, str | None]]) -> None:
        if tag.lower() != "script":
            return
        if any(name.lower() == "src" for name, _ in attrs):
            return
        self._collecting = True
        self._parts = []

    def handle_data(self, data: str) -> None:
        if self._collecting:
            self._parts.append(data)

    def handle_entityref(self, name: str) -> None:
        if self._collecting:
            self._parts.append(f"&{name};")

    def handle_charref(self, name: str) -> None:
        if self._collecting:
            self._parts.append(f"&#{name};")

    def handle_endtag(self, tag: str) -> None:
        if tag.lower() == "script" and self._collecting:
            self.scripts.append("".join(self._parts))
            self._collecting = False


def script_hashes(site_dir: pathlib.Path) -> list[str]:
    hashes: set[str] = set()
    for path in sorted(site_dir.rglob("*.html")):
        parser = InlineScriptParser()
        parser.feed(path.read_text(encoding="utf-8"))
        for script in parser.scripts:
            digest = hashlib.sha256(script.encode("utf-8")).digest()
            hashes.add("'sha256-" + base64.b64encode(digest).decode("ascii") + "'")
    return sorted(hashes)


def main() -> None:
    site_dir = pathlib.Path(sys.argv[1])
    output = pathlib.Path(sys.argv[2])
    script_sources = " ".join(script_hashes(site_dir))
    csp = "; ".join(
        [
            "default-src 'self'",
            f"script-src 'self' {script_sources}",
            "style-src 'self' 'unsafe-inline'",
            "img-src 'self' data:",
            "font-src 'none'",
            "connect-src 'self' https://api.github.com",
            "worker-src 'self'",
            "frame-src 'none'",
            "object-src 'none'",
            "base-uri 'self'",
            "form-action 'self'",
            "frame-ancestors 'none'",
            "upgrade-insecure-requests",
        ]
    )
    output.write_text(
        "\n".join(
            [
                'add_header Strict-Transport-Security "max-age=31536000; includeSubDomains; preload" always;',
                'add_header X-Content-Type-Options "nosniff" always;',
                'add_header Referrer-Policy "strict-origin-when-cross-origin" always;',
                'add_header X-Frame-Options "DENY" always;',
                'add_header Permissions-Policy "accelerometer=(), camera=(), geolocation=(), gyroscope=(), magnetometer=(), microphone=(), payment=(), usb=()" always;',
                f'add_header Content-Security-Policy "{csp}" always;',
                "",
            ]
        ),
        encoding="utf-8",
    )


if __name__ == "__main__":
    main()
