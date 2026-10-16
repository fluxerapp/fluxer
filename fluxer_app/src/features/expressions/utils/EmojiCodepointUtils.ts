// SPDX-License-Identifier: AGPL-3.0-or-later

const EYE_IN_SPEECH_BUBBLE = '\u{1F441}\u200D\u{1F5E8}';

export function convertToCodePoints(emoji: string): string {
	const emojiWithoutFE0F = emoji.replace(/\uFE0F/g, '');
	const keepsFE0F = emoji.includes('\u200D') && emojiWithoutFE0F !== EYE_IN_SPEECH_BUBBLE;
	const processedEmoji = keepsFE0F ? emoji : emojiWithoutFE0F;
	return Array.from(processedEmoji)
		.map((char) => char.codePointAt(0)?.toString(16).replace(/^0+/, '') || '')
		.join('-');
}
