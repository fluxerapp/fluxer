// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {
	applyDefaultAppShellDocumentBranding,
	applyRuntimeDocumentBranding,
} from '@app/features/app/state/RuntimeDocumentBranding';
import {http} from '@app/features/platform/transport/RestTransport';
import {compareStructural, reaction} from 'mobx';

let activeDisposer: (() => void) | null = null;

export function installRuntimeConfigEffects(): () => void {
	if (activeDisposer !== null) {
		throw new Error('Runtime config effects are already installed');
	}
	const disposeRouting = reaction(
		() => {
			const snapshot = RuntimeConfig.getSnapshotOrNull();
			if (snapshot === null) {
				return null;
			}
			return {
				baseUrl: RuntimeConfig.transportApiEndpoint,
				canonicalBaseUrl: snapshot.apiEndpoint,
				apiVersion: snapshot.apiCodeVersion,
			};
		},
		(routing) => {
			if (routing === null) {
				http.clearRuntime();
				return;
			}
			http.configure(routing);
		},
		{fireImmediately: true, equals: compareStructural},
	);
	const disposeBranding = reaction(
		() => RuntimeConfig.getSnapshotOrNull()?.appPublic ?? null,
		(appPublic) => {
			if (appPublic === null) {
				applyDefaultAppShellDocumentBranding();
				return;
			}
			applyRuntimeDocumentBranding(appPublic);
		},
		{fireImmediately: true, equals: compareStructural},
	);
	const dispose = (): void => {
		if (activeDisposer !== dispose) {
			throw new Error('Runtime config effects are not installed by this disposer');
		}
		disposeRouting();
		disposeBranding();
		http.clearRuntime();
		applyDefaultAppShellDocumentBranding();
		activeDisposer = null;
	};
	activeDisposer = dispose;
	return dispose;
}
