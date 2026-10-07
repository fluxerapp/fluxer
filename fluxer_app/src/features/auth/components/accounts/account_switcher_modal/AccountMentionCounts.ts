// SPDX-License-Identifier: AGPL-3.0-or-later

export interface AccountMentionCountSource {
	getAccountMentionCount(accountKey: string): number;
}

let mentionCountSource: AccountMentionCountSource | null = null;

export function setAccountMentionCountSource(source: AccountMentionCountSource | null): void {
	mentionCountSource = source;
}

export function getAccountMentionCount(accountKey: string): number {
	if (mentionCountSource == null) {
		return 0;
	}
	return mentionCountSource.getAccountMentionCount(accountKey);
}
