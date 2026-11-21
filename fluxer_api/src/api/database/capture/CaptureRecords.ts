// SPDX-License-Identifier: AGPL-3.0-or-later

import type {CaptureValue} from '@app/api/database/capture/CaptureCodec';

export const CAPTURE_SUBJECT_PREFIX = 'cassandra.capture';

export type CaptureProcess = 'api' | 'worker' | 'users';
export type CaptureOutcome = 'applied' | 'failed' | 'unknown' | 'not_applied';
export type CaptureScope = 'row' | 'partition' | 'prefix';
export type ProducerState = 'open' | 'closed' | 'sealed' | 'disabled';
export type SettleReason = 'lwt' | 'lwt_unknown' | 'unknown';

interface CaptureKey {
	s: CaptureScope;
	c: Record<string, CaptureValue>;
}

export interface CaptureStatement {
	t: string;
	q: string;
	p: Record<string, CaptureValue>;
	ttl?: number;
	keys?: Array<CaptureKey>;
}

interface CaptureLwtRow {
	t: string;
	c: Record<string, CaptureValue>;
	insert?: true;
}

export interface IntentRecord {
	kind: 'i';
	producer: string;
	seq: number;
	ts: number;
	logged: boolean;
	statements: Array<CaptureStatement>;
}

export interface LwtIntentRecord {
	kind: 'l';
	producer: string;
	seq: number;
	ts: number;
	rows: Array<CaptureLwtRow>;
}

export interface OutcomeRecord {
	kind: 'o';
	producer: string;
	outcomes: Array<[number, CaptureOutcome]>;
}

export interface LifeRecord {
	kind: 'life';
	producer: string;
	state: ProducerState;
	last_seq?: number;
}

export interface SettleRecord {
	kind: 'settle';
	t: string;
	s: CaptureScope;
	c: Record<string, CaptureValue>;
	reason: SettleReason;
	ts: number;
	insert?: true;
}

export type ProducerRecord = IntentRecord | LwtIntentRecord | OutcomeRecord | LifeRecord;

export function captureSubject(process: CaptureProcess, kind: 'i' | 'l' | 'o' | 'life'): string {
	return `${CAPTURE_SUBJECT_PREFIX}.${process}.${kind}`;
}
