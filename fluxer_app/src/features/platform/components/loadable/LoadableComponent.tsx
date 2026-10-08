// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/platform/components/loadable/LoadableComponent.module.css';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {
	attemptLazyModuleRecoveryReload,
	canAttemptLazyModuleRecoveryReload,
	isLazyModuleLoadError,
	loadLazyModule,
} from '@app/features/platform/utils/LazyModuleLoader';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {ArrowClockwiseIcon} from '@phosphor-icons/react';
import type React from 'react';
import {type ComponentType, useCallback, useEffect, useSyncExternalStore} from 'react';

const logger = new Logger('LoadableComponent');

export interface LoadableErrorProps {
	error: unknown;
	retry: () => void;
}

export interface LoadableModule<Props extends object> {
	default: ComponentType<Props>;
}

export type LoadableComponent<Props extends object> = ComponentType<Props> & {
	preload: () => Promise<boolean>;
};

export interface CreateLoadableComponentOptions<Props extends object> {
	displayName: string;
	load: () => Promise<LoadableModule<Props>>;
	LoadingComponent?: ComponentType;
	ErrorComponent?: ComponentType<LoadableErrorProps>;
}

interface CreateDefaultLoadableComponentOptions<Props extends object>
	extends Omit<CreateLoadableComponentOptions<Props>, 'load'> {
	load: () => Promise<{default: unknown}>;
}

interface CreateNamedLoadableComponentOptions<Props extends object>
	extends Omit<CreateLoadableComponentOptions<Props>, 'load'> {
	load: () => Promise<unknown>;
}

type LoadState<Props extends object> =
	| {
			status: 'idle' | 'loading';
	  }
	| {
			status: 'loaded';
			Component: ComponentType<Props>;
	  }
	| {
			status: 'error';
			error: unknown;
			speculative: boolean;
	  };

const INLINE_LOAD_ERROR_DESCRIPTOR = msg({
	message: "Couldn't load this part of the app. Check your connection and try again.",
	comment: 'Tooltip on the compact retry control shown where a lazily loaded component failed to load.',
});

function NullLoadingComponent(): null {
	return null;
}

function useLazyModuleRecovery(error: unknown, retry: () => void): {canReload: boolean; handleReload: () => void} {
	const canReload = isLazyModuleLoadError(error) && canAttemptLazyModuleRecoveryReload();
	const handleReload = useCallback(() => {
		if (!attemptLazyModuleRecoveryReload()) {
			retry();
		}
	}, [retry]);
	return {canReload, handleReload};
}

function LoadableInlineLoadError({error, retry}: LoadableErrorProps): React.JSX.Element {
	const {i18n} = useLingui();
	const {canReload, handleReload} = useLazyModuleRecovery(error, retry);
	return (
		<button
			type="button"
			className={styles.inline}
			title={i18n._(INLINE_LOAD_ERROR_DESCRIPTOR)}
			onClick={canReload ? handleReload : retry}
			data-flx="platform.loadable.loadable-component.loadable-inline-load-error.inline.reload.button"
		>
			<ArrowClockwiseIcon
				className={styles.inlineIcon}
				weight="bold"
				data-flx="platform.loadable.loadable-component.loadable-inline-load-error.inline-icon"
			/>
			<Trans>Retry</Trans>
		</button>
	);
}

export function createLoadableComponent<Props extends object>({
	displayName,
	load,
	LoadingComponent = NullLoadingComponent,
	ErrorComponent = LoadableInlineLoadError,
}: CreateLoadableComponentOptions<Props>): LoadableComponent<Props> {
	let state: LoadState<Props> = {status: 'idle'};
	let loadPromise: Promise<ComponentType<Props>> | null = null;
	const listeners = new Set<() => void>();

	const getState = (): LoadState<Props> => state;

	const subscribe = (listener: () => void): (() => void) => {
		listeners.add(listener);
		return () => {
			listeners.delete(listener);
		};
	};

	const setState = (next: LoadState<Props>): void => {
		state = next;
		for (const listener of listeners) {
			listener();
		}
	};

	const clearErrorState = (): void => {
		if (state.status === 'error') {
			setState({status: 'idle'});
		}
	};

	const loadOnce = async (): Promise<ComponentType<Props>> => {
		if (state.status === 'loaded') {
			return state.Component;
		}
		if (loadPromise) {
			return loadPromise;
		}
		if (state.status !== 'error') {
			setState({status: 'loading'});
		}
		loadPromise = loadLazyModule(load)
			.then((module) => {
				setState({
					status: 'loaded',
					Component: module.default,
				});
				return module.default;
			})
			.catch((error: unknown) => {
				setState({
					status: 'error',
					error,
					speculative: listeners.size === 0,
				});
				logger.error(`Failed to load ${displayName}`, error);
				throw error;
			})
			.finally(() => {
				loadPromise = null;
			});
		return loadPromise;
	};

	const preload = async (): Promise<boolean> => {
		try {
			await loadOnce();
			return true;
		} catch {
			return false;
		}
	};

	const Loadable = (props: Props) => {
		const current = useSyncExternalStore(subscribe, getState, getState);
		useEffect(() => {
			if (state.status === 'loaded') {
				return;
			}
			if (state.status === 'error' && !state.speculative) {
				return;
			}
			void loadOnce().catch(() => {});
		}, []);
		const retry = useCallback(() => {
			clearErrorState();
			void loadOnce().catch(() => {});
		}, []);
		if (current.status === 'loaded') {
			const LoadedComponent = current.Component;
			return <LoadedComponent data-flx="platform.loadable.loadable-component.loaded-component" {...props} />;
		}
		if (current.status === 'error') {
			return (
				<ErrorComponent
					error={current.error}
					retry={retry}
					data-flx="platform.loadable.loadable-component.error-component"
				/>
			);
		}
		return <LoadingComponent data-flx="platform.loadable.loadable-component.loading-component" />;
	};

	Loadable.displayName = displayName;
	return Object.assign(Loadable, {preload});
}

export function createDefaultLoadableComponent<Props extends object>({
	load,
	...options
}: CreateDefaultLoadableComponentOptions<Props>): LoadableComponent<Props> {
	return createLoadableComponent<Props>({
		...options,
		load: async () => {
			const module = await load();
			return {default: module.default as ComponentType<Props>};
		},
	});
}

export function createNamedLoadableComponent<Props extends object>({
	load,
	...options
}: CreateNamedLoadableComponentOptions<Props>): LoadableComponent<Props> {
	return createLoadableComponent<Props>({
		...options,
		load: async () => ({default: (await load()) as ComponentType<Props>}),
	});
}
