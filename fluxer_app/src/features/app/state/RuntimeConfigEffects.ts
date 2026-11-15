// SPDX-License-Identifier: AGPL-3.0-or-later

import {runtimeInstanceKey} from '@app/features/app/state/InstanceSnapshotStore';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {
	applyRuntimeDocumentBranding,
	applyServedDocumentBranding,
} from '@app/features/app/state/RuntimeDocumentBranding';
import {http} from '@app/features/platform/transport/RestTransport';
import {getElectronAPI} from '@app/features/ui/utils/NativeUtils';
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
				applyServedDocumentBranding();
				return;
			}
			applyRuntimeDocumentBranding(appPublic);
		},
		{fireImmediately: true, equals: compareStructural},
	);
	const disposeDesktopProductName = reaction(
		() => {
			const snapshot = RuntimeConfig.getSnapshotOrNull();
			if (snapshot === null) {
				return null;
			}
			const instanceKey = runtimeInstanceKey(snapshot);
			if (instanceKey === null) {
				return null;
			}
			return {instanceKey, productName: snapshot.appPublic.branding.product_name};
		},
		(report) => {
			if (report === null) {
				return;
			}
			getElectronAPI()?.reportInstanceProductName?.(report.instanceKey, report.productName);
		},
		{fireImmediately: true, equals: compareStructural},
	);
	const dispose = (): void => {
		if (activeDisposer !== dispose) {
			throw new Error('Runtime config effects are not installed by this disposer');
		}
		disposeRouting();
		disposeBranding();
		disposeDesktopProductName();
		http.clearRuntime();
		applyServedDocumentBranding();
		activeDisposer = null;
	};
	activeDisposer = dispose;
	return dispose;
}
