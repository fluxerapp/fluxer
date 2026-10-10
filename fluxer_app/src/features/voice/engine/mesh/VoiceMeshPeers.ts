// SPDX-License-Identifier: AGPL-3.0-or-later

import {makeAutoObservable} from 'mobx';

export type VoiceMeshPeerStatus = 'connecting' | 'connected' | 'failed';

class VoiceMeshPeers {
	statuses: Readonly<Record<string, VoiceMeshPeerStatus>> = {};

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	get failedConnectionIds(): ReadonlyArray<string> {
		return Object.keys(this.statuses).filter((connectionId) => this.statuses[connectionId] === 'failed');
	}

	get hasFailedPeer(): boolean {
		return this.failedConnectionIds.length > 0;
	}

	getStatus(connectionId: string): VoiceMeshPeerStatus | null {
		return this.statuses[connectionId] ?? null;
	}

	setStatus(connectionId: string, status: VoiceMeshPeerStatus): void {
		if (this.statuses[connectionId] === status) return;
		this.statuses = {...this.statuses, [connectionId]: status};
	}

	remove(connectionId: string): void {
		if (!Object.hasOwn(this.statuses, connectionId)) return;
		const {[connectionId]: _removed, ...rest} = this.statuses;
		this.statuses = rest;
	}

	reset(): void {
		this.statuses = {};
	}
}

export default new VoiceMeshPeers();
