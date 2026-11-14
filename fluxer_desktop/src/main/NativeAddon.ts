// SPDX-License-Identifier: AGPL-3.0-or-later

interface OptionalNativeAddonOptions {
	readonly platform: NodeJS.Platform;
	readonly disabled?: () => boolean;
	readonly load: () => unknown;
	readonly logger: {info: (...args: Array<unknown>) => void; warn: (...args: Array<unknown>) => void};
	readonly level: 'info' | 'warn';
	readonly unavailableMessage: string;
	readonly disabledMessage?: string;
}

export function loadOptionalNativeAddon<T extends {loadError?: Error | null}>(
	options: OptionalNativeAddonOptions,
): () => T | null {
	let cached: T | null | undefined;
	return () => {
		if (cached !== undefined) return cached;
		if (process.platform !== options.platform || options.disabled?.()) {
			if (process.platform === options.platform && options.disabledMessage) {
				options.logger[options.level](options.disabledMessage);
			}
			return (cached = null);
		}
		try {
			const addon = options.load() as T;
			if (addon.loadError) throw addon.loadError;
			return (cached = addon);
		} catch (error) {
			options.logger[options.level](options.unavailableMessage, {error});
			return (cached = null);
		}
	};
}
