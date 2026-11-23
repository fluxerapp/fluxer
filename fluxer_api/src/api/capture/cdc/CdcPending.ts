// SPDX-License-Identifier: AGPL-3.0-or-later

import type {CaptureValue} from '@app/api/database/capture/CaptureCodec';
import type {CaptureScope} from '@app/api/database/capture/CaptureRecords';

export interface PendingInput {
	table: string;
	scope: CaptureScope;
	key: Record<string, CaptureValue>;
	primaryKey: ReadonlyArray<string>;
	ts: number;
	serial: boolean;
	unknown: boolean;
	recheckDue?: string;
}

export interface PendingKey {
	id: string;
	table: string;
	scope: CaptureScope;
	key: Record<string, CaptureValue>;
	ts: number;
	serial: boolean;
	unknown: boolean;
	recheckDue: string | null;
}

function canonicalKey(primaryKey: ReadonlyArray<string>, key: Readonly<Record<string, CaptureValue>>): string {
	return JSON.stringify(primaryKey.filter((column) => column in key).map((column) => key[column]));
}

export class PendingKeys {
	private readonly keys = new Map<string, PendingKey>();

	get size(): number {
		return this.keys.size;
	}

	add(input: PendingInput): void {
		const id = `${input.table}\u0000${input.scope}\u0000${canonicalKey(input.primaryKey, input.key)}`;
		const existing = this.keys.get(id);
		if (existing === undefined) {
			this.keys.set(id, {
				id,
				table: input.table,
				scope: input.scope,
				key: input.key,
				ts: input.ts,
				serial: input.serial,
				unknown: input.unknown,
				recheckDue: input.recheckDue ?? null,
			});
			return;
		}
		existing.ts = Math.max(existing.ts, input.ts);
		existing.serial ||= input.serial;
		existing.unknown ||= input.unknown;
		if (input.recheckDue !== undefined && (existing.recheckDue ?? '') < input.recheckDue) {
			existing.recheckDue = input.recheckDue;
		}
	}

	take(): Array<PendingKey> {
		const keys = [...this.keys.values()];
		this.keys.clear();
		return keys;
	}
}
