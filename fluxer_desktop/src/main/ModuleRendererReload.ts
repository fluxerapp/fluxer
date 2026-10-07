// SPDX-License-Identifier: AGPL-3.0-or-later

const RENDERER_RELOAD_TIMEOUT_MS = 60_000;

interface ModuleRendererReloadTarget {
	once(event: 'destroyed', listener: () => void): void;
	removeListener(event: 'destroyed', listener: () => void): void;
}

interface ModuleRendererReloadWatch {
	readonly target: ModuleRendererReloadTarget;
	readonly observeLaunchConfirmed: (listener: () => void) => () => void;
	readonly timeoutMs?: number;
	readonly onLoaded: () => void;
	readonly onAbandoned: (reason: string) => void;
}

export function watchModuleRendererReload({
	target,
	observeLaunchConfirmed,
	timeoutMs = RENDERER_RELOAD_TIMEOUT_MS,
	onLoaded,
	onAbandoned,
}: ModuleRendererReloadWatch): () => void {
	let abandoned = false;
	let deadline: ReturnType<typeof setTimeout> | null = null;
	let stopObservingConfirmation: (() => void) | null = null;
	const clearDeadline = (): void => {
		if (deadline != null) {
			clearTimeout(deadline);
			deadline = null;
		}
	};
	const stopConfirmation = (): void => {
		const stop = stopObservingConfirmation;
		stopObservingConfirmation = null;
		stop?.();
	};
	const abandon = (reason: string): void => {
		clearDeadline();
		if (abandoned) {
			return;
		}
		abandoned = true;
		onAbandoned(reason);
	};
	const handleLoaded = (): void => {
		clearDeadline();
		stopConfirmation();
		target.removeListener('destroyed', handleDestroyed);
		onLoaded();
	};
	const handleDestroyed = (): void => {
		stopConfirmation();
		abandon('the renderer was destroyed before it confirmed the reload');
	};
	stopObservingConfirmation = observeLaunchConfirmed(handleLoaded);
	target.once('destroyed', handleDestroyed);
	deadline = setTimeout(() => {
		deadline = null;
		abandon('the renderer did not confirm the reload in time');
	}, timeoutMs);
	deadline.unref();
	return (): void => {
		clearDeadline();
		stopConfirmation();
		target.removeListener('destroyed', handleDestroyed);
	};
}
