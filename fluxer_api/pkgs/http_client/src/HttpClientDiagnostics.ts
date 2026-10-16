export function formatUrlForDiagnostics(value: string | URL): string {
	const url = typeof value === 'string' ? URL.parse(value) : value;
	if (!url) {
		return '[invalid URL]';
	}
	if (url.protocol !== 'http:' && url.protocol !== 'https:') {
		return '[non-HTTP URL]';
	}
	return url.origin;
}
