// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
export enum DataChannelKind {
	RELIABLE = 0,
	LOSSY = 1,
	DATA_TRACK_LOSSY = 2,
}

export const reliableDataChannelWaterMarkLow = 64 * 1024;
export const reliableDataChannelWaterMarkHigh = 1024 * 1024;
export const lossyDataChannelWaterMarkLow = 8 * 1024;
export const lossyDataChannelWaterMarkHigh = 256 * 1024;

export function dataChannelLowWaterMark(kind: DataChannelKind): number {
	return kind === DataChannelKind.RELIABLE ? reliableDataChannelWaterMarkLow : lossyDataChannelWaterMarkLow;
}

export function dataChannelHighWaterMark(kind: DataChannelKind): number {
	return kind === DataChannelKind.RELIABLE ? reliableDataChannelWaterMarkHigh : lossyDataChannelWaterMarkHigh;
}
