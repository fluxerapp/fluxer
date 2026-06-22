// SPDX-License-Identifier: AGPL-3.0-or-later

export function resolveActivityImageUrl(image: string | undefined, applicationId?: string): string | null {
	if (!image) return null;
	if (image.startsWith('http://') || image.startsWith('https://')) return image;

	const colonIndex = image.indexOf(':');
	if (colonIndex === -1) {
		if (!applicationId) return null;
		return null;
	}

	const platform = image.slice(0, colonIndex);
	const id = image.slice(colonIndex + 1);
	switch (platform) {
		case 'spotify':
			return `https://i.scdn.co/image/${id}`;
		case 'youtube':
			return `https://i.ytimg.com/vi/${id}/hqdefault_live.jpg`;
		case 'twitch':
			return `https://static-cdn.jtvnw.net/previews-ttv/live_user_${id}.png`;
		default:
			return null;
	}
}
