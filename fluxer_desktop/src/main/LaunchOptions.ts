// SPDX-License-Identifier: AGPL-3.0-or-later

type LinuxInputHookMode = 'auto' | 'off' | 'evdev' | 'native';
type LaunchToggleMode = 'auto' | 'off';
type NativeNotificationsMode = 'auto' | 'off' | 'native' | 'electron';
export type SpellcheckLaunchMode = 'default' | 'off' | 'auto' | 'hunspell' | 'system';

const PORTABLE_MODE_ARGS = new Set(['--fluxer-portable']);
const SAFE_MODE_ARGS = new Set(['--fluxer-safe-mode']);
const DISABLE_INPUT_HOOK_ARGS = new Set([
	'--fluxer-disable-native-input',
	'--fluxer-disable-input-hooks',
	'--fluxer-disable-global-key-hook',
]);
const INPUT_HOOK_MODE_ARGS = new Set(['--fluxer-input-hook', '--fluxer-native-input']);
const DISABLE_LINUX_PORTALS_ARGS = new Set(['--fluxer-disable-linux-portals']);
const LINUX_PORTALS_MODE_ARGS = new Set(['--fluxer-linux-portals', '--fluxer-portals']);
const DISABLE_NATIVE_NOTIFICATIONS_ARGS = new Set(['--fluxer-disable-native-notifications']);
const NATIVE_NOTIFICATIONS_MODE_ARGS = new Set(['--fluxer-native-notifications', '--fluxer-notifications']);
const DISABLE_NATIVE_AUDIO_ARGS = new Set(['--fluxer-disable-native-audio']);
const NATIVE_AUDIO_MODE_ARGS = new Set(['--fluxer-native-audio']);
const DISABLE_SPELLCHECK_ARGS = new Set(['--fluxer-disable-spellcheck', '--fluxer-disable-linux-spellcheck']);
const SPELLCHECK_MODE_ARGS = new Set(['--fluxer-spellcheck']);
const DISABLE_V8_CODE_CACHE_ARGS = new Set(['--fluxer-disable-v8-code-cache']);
const DISABLE_NATIVE_GATEWAY_ARGS = new Set(['--fluxer-disable-native-gateway']);
const DESKTOP_UPDATE_ARGS = new Set(['--fluxer-update']);

function hasFlag(argv: ReadonlyArray<string>, flags: ReadonlySet<string>): boolean {
	return argv.some((arg) => flags.has(arg) || [...flags].some((flag) => arg.startsWith(`${flag}=`)));
}

function getArgValue(argv: ReadonlyArray<string>, names: ReadonlySet<string>): string | null {
	for (let index = 0; index < argv.length; index += 1) {
		const arg = argv[index];
		for (const name of names) {
			if (arg === name) {
				const next = argv[index + 1];
				return next && !next.startsWith('--') ? next : '';
			}
			if (arg.startsWith(`${name}=`)) {
				return arg.slice(name.length + 1);
			}
		}
	}
	return null;
}

function normalizeMode<T extends string>(
	value: string | null | undefined,
	allowed: ReadonlyArray<T>,
	optionName: string,
): T | null {
	if (value == null) return null;
	if (value.trim() === '') throw new Error(`${optionName} requires a value: ${allowed.join(', ')}`);
	const normalized = value.trim().toLowerCase();
	if ((allowed as ReadonlyArray<string>).includes(normalized)) return normalized as T;
	throw new Error(`${optionName} must be one of: ${allowed.join(', ')}`);
}

function truthyEnv(name: string): boolean {
	const value = process.env[name];
	if (!value) return false;
	return ['1', 'true', 'yes', 'on'].includes(value.trim().toLowerCase());
}

function envMode<T extends string>(name: string, allowed: ReadonlyArray<T>): T | null {
	return normalizeMode(process.env[name], allowed, name);
}

export function isSafeModeLaunch(argv: ReadonlyArray<string> = process.argv): boolean {
	return hasFlag(argv, SAFE_MODE_ARGS);
}

export function getLinuxInputHookMode(argv: ReadonlyArray<string> = process.argv): LinuxInputHookMode {
	if (isSafeModeLaunch(argv) || hasFlag(argv, DISABLE_INPUT_HOOK_ARGS) || truthyEnv('FLUXER_DISABLE_NATIVE_INPUT')) {
		return 'off';
	}
	const argMode = normalizeMode(
		getArgValue(argv, INPUT_HOOK_MODE_ARGS),
		['auto', 'off', 'evdev', 'native'],
		'--fluxer-input-hook',
	);
	return argMode ?? envMode('FLUXER_INPUT_HOOK', ['auto', 'off', 'evdev', 'native']) ?? 'auto';
}

export function getLinuxPortalsMode(argv: ReadonlyArray<string> = process.argv): LaunchToggleMode {
	if (
		isSafeModeLaunch(argv) ||
		hasFlag(argv, DISABLE_LINUX_PORTALS_ARGS) ||
		truthyEnv('FLUXER_DISABLE_LINUX_PORTALS')
	) {
		return 'off';
	}
	const argMode = normalizeMode(getArgValue(argv, LINUX_PORTALS_MODE_ARGS), ['auto', 'off'], '--fluxer-linux-portals');
	return argMode ?? envMode('FLUXER_LINUX_PORTALS', ['auto', 'off']) ?? 'auto';
}

export function getNativeNotificationsMode(argv: ReadonlyArray<string> = process.argv): NativeNotificationsMode {
	if (
		isSafeModeLaunch(argv) ||
		hasFlag(argv, DISABLE_NATIVE_NOTIFICATIONS_ARGS) ||
		truthyEnv('FLUXER_DISABLE_NATIVE_NOTIFICATIONS')
	) {
		return 'off';
	}
	const argMode = normalizeMode(
		getArgValue(argv, NATIVE_NOTIFICATIONS_MODE_ARGS),
		['auto', 'off', 'native', 'electron'],
		'--fluxer-native-notifications',
	);
	return argMode ?? envMode('FLUXER_NATIVE_NOTIFICATIONS', ['auto', 'off', 'native', 'electron']) ?? 'auto';
}

export function getNativeAudioMode(argv: ReadonlyArray<string> = process.argv): LaunchToggleMode {
	if (isSafeModeLaunch(argv) || hasFlag(argv, DISABLE_NATIVE_AUDIO_ARGS) || truthyEnv('FLUXER_DISABLE_NATIVE_AUDIO')) {
		return 'off';
	}
	const argMode = normalizeMode(getArgValue(argv, NATIVE_AUDIO_MODE_ARGS), ['auto', 'off'], '--fluxer-native-audio');
	return argMode ?? envMode('FLUXER_NATIVE_AUDIO', ['auto', 'off']) ?? 'auto';
}

export function getSpellcheckLaunchMode(argv: ReadonlyArray<string> = process.argv): SpellcheckLaunchMode {
	if (isSafeModeLaunch(argv) || hasFlag(argv, DISABLE_SPELLCHECK_ARGS) || truthyEnv('FLUXER_DISABLE_SPELLCHECK')) {
		return 'off';
	}
	const argMode = normalizeMode(
		getArgValue(argv, SPELLCHECK_MODE_ARGS),
		['off', 'auto', 'hunspell', 'system'],
		'--fluxer-spellcheck',
	);
	return argMode ?? envMode('FLUXER_SPELLCHECK', ['off', 'auto', 'hunspell', 'system']) ?? 'default';
}

export function shouldDisableV8CodeCache(argv: ReadonlyArray<string> = process.argv): boolean {
	return (
		isSafeModeLaunch(argv) || hasFlag(argv, DISABLE_V8_CODE_CACHE_ARGS) || truthyEnv('FLUXER_DISABLE_V8_CODE_CACHE')
	);
}

export function getNativeGatewayDisableReason(argv: ReadonlyArray<string> = process.argv): string | null {
	if (isSafeModeLaunch(argv)) return '--fluxer-safe-mode';
	if (hasFlag(argv, DISABLE_NATIVE_GATEWAY_ARGS)) return '--fluxer-disable-native-gateway';
	if (truthyEnv('FLUXER_DISABLE_NATIVE_GATEWAY')) return 'FLUXER_DISABLE_NATIVE_GATEWAY';
	return null;
}

function isPortableLaunchFlag(argv: ReadonlyArray<string> = process.argv): boolean {
	return hasFlag(argv, PORTABLE_MODE_ARGS);
}

export function describeLaunchDiagnosticOptions(argv: ReadonlyArray<string> = process.argv): Record<string, unknown> {
	return {
		portableFlag: isPortableLaunchFlag(argv),
		safeMode: isSafeModeLaunch(argv),
		linuxInputHook: getLinuxInputHookMode(argv),
		linuxPortals: getLinuxPortalsMode(argv),
		nativeNotifications: getNativeNotificationsMode(argv),
		nativeAudio: getNativeAudioMode(argv),
		spellcheck: getSpellcheckLaunchMode(argv),
		v8CodeCacheDisabled: shouldDisableV8CodeCache(argv),
		nativeGatewayDisableReason: getNativeGatewayDisableReason(argv),
	};
}

export function isDesktopUpdateRequested(argv: ReadonlyArray<string> = process.argv): boolean {
	return hasFlag(argv, DESKTOP_UPDATE_ARGS);
}
