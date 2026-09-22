// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import * as RegexUtils from '@app/api/utils/RegexUtils';

let _invitePattern: RegExp | null = null;

function getInviteEndpointBase(): string {
	const url = new URL(Config.endpoints.invite);
	return `${url.hostname}${url.pathname.replace(/\/+$/, '')}`;
}

function getInvitePattern(): RegExp {
	if (!_invitePattern) {
		_invitePattern = new RegExp(
			[
				'(?:https?:\\/\\/)?',
				'(?:',
				`${RegexUtils.escapeRegex(getInviteEndpointBase())}(?:\\/#)?\\/(?!invite\\/)([a-zA-Z0-9\\-]{2,32})(?![a-zA-Z0-9\\-])`,
				'|',
				`${RegexUtils.escapeRegex(new URL(Config.endpoints.webApp).hostname)}(?:\\/#)?\\/invite\\/([a-zA-Z0-9\\-]{2,32})(?![a-zA-Z0-9\\-])`,
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
