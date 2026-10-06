// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import * as RegexUtils from '@app/api/utils/RegexUtils';

let _invitePattern: RegExp | null = null;

function getInviteEndpointBase(): string {
	const url = new URL(Config.endpoints.invite);
	return `${url.hostname}${url.pathname.replace(/\/+$/, '')}`;
}

function getWebAppHostsPattern(): string {
	const hostnames = new Set(Config.endpoints.webAppOrigins.map((origin) => new URL(origin).hostname));
	return [...hostnames].map((hostname) => RegexUtils.escapeRegex(hostname)).join('|');
}

function getInvitePattern(): RegExp {
	if (!_invitePattern) {
		_invitePattern = new RegExp(
			[
				'(?:https?:\\/\\/)?',
				'(?:',
				`${RegexUtils.escapeRegex(getInviteEndpointBase())}(?:\\/#)?\\/(?!invite\\/)([a-zA-Z0-9\\-]{2,32})(?![a-zA-Z0-9\\-])`,
				'|',
				`(?:${getWebAppHostsPattern()})(?:\\/#)?\\/invite\\/([a-zA-Z0-9\\-]{2,32})(?![a-zA-Z0-9\\-])`,
				')',
			].join(''),
			'gi',
		);
	}
	return _invitePattern;
}

export function findInvite(content: string | null): string | null {
	if (!content) return null;
	const pattern = getInvitePattern();
	pattern.lastIndex = 0;
	const match = pattern.exec(content);
	if (match) {
		return match[1] || match[2];
	}
	return null;
}

export function findInvites(content: string | null, limit: number): Array<string> {
	if (!content) return [];
	const pattern = getInvitePattern();
	pattern.lastIndex = 0;
	const codes = new Set<string>();
	for (const match of content.matchAll(pattern)) {
		const code = match[1] || match[2];
		if (code) codes.add(code);
		if (codes.size >= limit) break;
	}
	return [...codes];
}
