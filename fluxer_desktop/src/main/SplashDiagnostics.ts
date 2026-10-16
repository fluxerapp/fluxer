// SPDX-License-Identifier: AGPL-3.0-or-later

export interface SplashDiagnosticsInput {
	readonly generatedAt: number;
	readonly splashOpenedAt: number | null;
	readonly appVersion: string;
	readonly channel: string;
	readonly platform: string;
	readonly arch: string;
	readonly osVersion: string;
	readonly electronVersion: string;
	readonly logsPath: string | null;
	readonly userDataPath: string;
	readonly packageOrigin: string;
	readonly proxyRoute: string | null;
	readonly splashStatus: string | null;
	readonly updaterStatus: string | null;
	readonly pendingModule: string | null;
	readonly receivedBytes: number | null;
	readonly totalBytes: number | null;
	readonly bytesPerSecond: number | null;
	readonly committed: Readonly<Record<string, string>>;
	readonly lastError: string | null;
	readonly lastErrorAt: number | null;
}

const SHA_PREFIX_LENGTH = 12;
const SECRET_HEADER_PATTERN =
	/\b(authorization|cookie|token|secret|password)\b\s*[:=]\s*(?:(?:bearer|basic)\s+)?[^\s&]+/giu;
const SECRET_QUERY_PATTERN = /([?&])([^=&#\s]+)=([^&#\s]*)/gu;
const SECRET_USERINFO_PATTERN = /[^\s:@/]+:[^\s@/]+@/gu;

export function redactDiagnosticText(value: string): string {
	return value
		.replace(SECRET_HEADER_PATTERN, (_match, key: string) => `${key}=…`)
		.replace(SECRET_QUERY_PATTERN, (_match, separator: string, key: string) => `${separator}${key}=…`)
		.replace(SECRET_USERINFO_PATTERN, '…@');
}

function formatTime(value: number | null): string {
	return value == null ? 'none' : new Date(value).toISOString();
}

function formatBytes(value: number | null): string {
	return value == null ? 'unknown' : `${value} bytes`;
}

export function buildSplashDiagnosticsText(input: SplashDiagnosticsInput): string {
	const committed = Object.entries(input.committed)
		.sort(([left], [right]) => left.localeCompare(right))
		.map(([name, sha]) => `  ${name} ${sha.slice(0, SHA_PREFIX_LENGTH)}`);
	const lines = [
		'Fluxer desktop updater diagnostics',
		`Generated: ${formatTime(input.generatedAt)}`,
		`Splash opened: ${formatTime(input.splashOpenedAt)}`,
		`App version: ${input.appVersion} (${input.channel})`,
		`Platform: ${input.platform} ${input.arch}, OS ${input.osVersion}, Electron ${input.electronVersion}`,
		`Logs: ${input.logsPath ?? 'unknown'}`,
		`User data: ${input.userDataPath}`,
		`Package origin: ${input.packageOrigin}`,
		`Proxy route: ${input.proxyRoute ?? 'unknown'}`,
		`Splash status: ${input.splashStatus ?? 'unknown'}`,
		`Updater status: ${input.updaterStatus ?? 'unknown'}`,
		`Pending module: ${input.pendingModule ?? 'none'}`,
		`Downloaded: ${formatBytes(input.receivedBytes)} of ${formatBytes(input.totalBytes)}`,
		`Speed: ${input.bytesPerSecond == null ? 'unknown' : `${Math.round(input.bytesPerSecond)} bytes/s`}`,
		`Last updater error: ${input.lastError ?? 'none'}`,
		`Last updater error at: ${formatTime(input.lastErrorAt)}`,
		'Committed modules:',
		...(committed.length === 0 ? ['  none'] : committed),
	];
	return redactDiagnosticText(lines.join('\n'));
}
