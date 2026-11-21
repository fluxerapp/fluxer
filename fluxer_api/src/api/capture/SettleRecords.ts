// SPDX-License-Identifier: AGPL-3.0-or-later

import type {CaptureValue} from '@app/api/database/capture/CaptureCodec';
import {CAPTURE_SUBJECT_PREFIX, type CaptureScope} from '@app/api/database/capture/CaptureRecords';

export const SETTLE_SUBJECT = `${CAPTURE_SUBJECT_PREFIX}.settle`;
export const IMAGE_SUBJECT = `${CAPTURE_SUBJECT_PREFIX}.img`;
export const PASS_SUBJECT = `${CAPTURE_SUBJECT_PREFIX}.pass`;
export const MARK_SUBJECT = `${CAPTURE_SUBJECT_PREFIX}.mark`;

interface ImageCell {
	v: CaptureValue;
	wt: number;
	ttl: number | null;
}

interface ImageRow {
	c: Record<string, CaptureValue>;
	cells: Record<string, ImageCell>;
}

export interface ImageRecord {
	kind: 'img';
	pass: string;
	h: number;
	read_at: number;
	t: string;
	s: CaptureScope;
	c: Record<string, CaptureValue>;
	rows: Array<ImageRow>;
	insert?: true;
	verify?: true;
	unreadable?: string;
}

export interface PassRecord {
	kind: 'pass';
	pass: string;
	state: 'begin' | 'done';
	h: number;
	prev_h: number;
	scan_from: number;
	keys?: number;
	seconds?: number;
	verify?: true;
}

export interface MarkRecord {
	kind: 'mark';
	watermark: number | null;
	drained: boolean;
}
