// SPDX-License-Identifier: AGPL-3.0-or-later

import {Connection} from '@app/features/connection/models/Connection';
import type {ConnectionType} from '@fluxer/constants/src/ConnectionConstants';
import type {ConnectionResponse} from '@fluxer/schema/src/domains/connection/ConnectionSchemas';
import {makeAutoObservable} from 'mobx';

class UserConnection {
	connections: Map<string, Connection> = new Map();
	fetched: boolean = false;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	setConnections(connections: ReadonlyArray<ConnectionResponse>): void {
		this.connections.clear();
		for (const connection of connections) {
			this.connections.set(connection.id, new Connection(connection));
		}
		this.fetched = true;
	}

	addConnection(connection: ConnectionResponse): void {
		this.connections.set(connection.id, new Connection(connection));
	}

	updateConnection(id: string, data: Partial<ConnectionResponse>): void {
		const existing = this.connections.get(id);
		if (!existing) return;
		const updated = {
			...existing.toJSON(),
			...data,
		};
		this.connections.set(id, new Connection(updated));
	}

	removeConnection(id: string): void {
		this.connections.delete(id);
	}

	getConnections(): ReadonlyArray<Connection> {
		return Array.from(this.connections.values()).sort((a, b) => a.sortOrder - b.sortOrder);
	}

	getConnection(id: string): Connection | undefined {
		return this.connections.get(id);
	}

	hasConnectionByTypeAndName(type: ConnectionType, name: string): boolean {
		const lowerName = name.toLowerCase();
		for (const connection of this.connections.values()) {
			if (connection.type === type && connection.name.toLowerCase() === lowerName) {
				return true;
			}
		}
		return false;
	}

	reset(): void {
		this.connections.clear();
		this.fetched = false;
	}

	handleGatewayReady(): void {
		this.reset();
	}
}

export default new UserConnection();
