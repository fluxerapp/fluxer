// SPDX-License-Identifier: AGPL-3.0-or-later

const PARTICIPANT_PATTERN = /^user_(\d+)_(.+)$/;

export function parseParticipantUserId(identity: string): string | null {
	return PARTICIPANT_PATTERN.exec(identity)?.[1] ?? null;
}

export function isTrustedMusicSender(identity: string, isBotUser: (userId: string) => boolean): boolean {
	const userId = parseParticipantUserId(identity);
	return userId !== null && isBotUser(userId);
}
