// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHmac} from 'node:crypto';
import type {UserID} from '@app/api/BrandedTypes';
import {Logger} from '@app/api/Logger';
import {deriveOperatorPassSecret} from '@app/api/operator_pass/OperatorPassSecrets';
import type {INatsConnectionManager} from '@pkgs/nats/src/INatsConnectionManager';

const OPERATOR_PASS_WALL_NATS_SUBJECT = 'operator_pass.wall.delta';
const OPERATOR_PASS_WALL_PUBLIC_ID_LABEL = 'fluxer-operator-pass-wall-public-id-v1';

export interface OperatorPassWallBlobEntry {
	id: string;
	name: string;
	avatar: string | null;
	tier: number;
	count?: number;
	business?: true;
}

export interface OperatorPassWallDelta {
	type: 'operator_pass_wall_delta';
	version: number;
	previous_version: number;
	published_at: string;
	resync: boolean;
	upserts: Array<OperatorPassWallBlobEntry>;
	removals: Array<string>;
}

export interface IOperatorPassWallPublisher {
	publish(delta: OperatorPassWallDelta): Promise<void>;
}

export function operatorPassWallPublicId(userId: UserID): string {
	return createHmac('sha256', deriveOperatorPassSecret(OPERATOR_PASS_WALL_PUBLIC_ID_LABEL))
		.update(userId.toString())
		.digest('hex')
		.slice(0, 16);
}

const textEncoder = new TextEncoder();

export class NatsOperatorPassWallPublisher implements IOperatorPassWallPublisher {
	constructor(private readonly connectionManager: INatsConnectionManager) {}

	async publish(delta: OperatorPassWallDelta): Promise<void> {
		try {
			if (this.connectionManager.isClosed()) await this.connectionManager.connect();
			const connection = this.connectionManager.getConnection();
			connection.publish(OPERATOR_PASS_WALL_NATS_SUBJECT, textEncoder.encode(JSON.stringify(delta)));
			await connection.flush();
		} catch (error) {
			Logger.warn({error, version: delta.version}, "Failed to publish an Operators' wall delta");
		}
	}
}
