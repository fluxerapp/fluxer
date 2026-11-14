// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import type {FrameMetadata} from '../../frameMetadata/types.ts';

const MAX_ENTRIES = 300;

export class FrameMetadataExtractor {
	private metadataMap = new Map<number, FrameMetadata>();

	private activeSsrc: number = 0;

	storeMetadata(rtpTimestamp: number, ssrc: number, metadata: FrameMetadata) {
		if (this.activeSsrc !== 0 && this.activeSsrc !== ssrc) {
			this.metadataMap.clear();
		}
		this.activeSsrc = ssrc;

		while (this.metadataMap.size >= MAX_ENTRIES) {
			const evicted = this.metadataMap.keys().next().value!;
			this.metadataMap.delete(evicted);
		}

		this.metadataMap.set(rtpTimestamp, metadata);
	}

	lookupMetadata(rtpTimestamp: number): FrameMetadata | undefined {
		return this.metadataMap.get(rtpTimestamp);
	}

	dispose() {
		this.metadataMap.clear();
		this.activeSsrc = 0;
	}
}
