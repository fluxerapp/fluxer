// SPDX-License-Identifier: AGPL-3.0-or-later

const CHANNEL_NAME_PATTERN = /(?<![\w/&#])#[a-z][a-z0-9-]*/g;

export function isolateBidi(text: string): string {
	return `⁨${text}⁩`;
}

export function isolateChannelNames(text: string): string {
	return text.replace(CHANNEL_NAME_PATTERN, (channel) => `⁦${channel}⁩`);
}
