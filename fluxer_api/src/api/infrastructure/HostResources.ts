// SPDX-License-Identifier: AGPL-3.0-or-later

import {availableParallelism, totalmem} from 'node:os';
import {getHeapStatistics} from 'node:v8';
import type {LoggerInterface} from '@fluxer/logger/src/LoggerInterface';
import sharp from 'sharp';

const MIB = 1024 * 1024;
const SHARP_CACHE_MEMORY_MB = 8;
const SHARP_CACHE_ITEMS = 20;

export interface HostResources {
	cores: number;
	memoryBytes: number;
}

function detectHostResources(): HostResources {
	const total = totalmem();
	const constrained = process.constrainedMemory();
	return {
		cores: availableParallelism(),
		memoryBytes: constrained > 0 ? Math.min(constrained, total) : total,
	};
}

export function sizeProcessForHost(logger: Pick<LoggerInterface, 'info'>): HostResources {
	const resources = detectHostResources();
	sharp.cache({memory: SHARP_CACHE_MEMORY_MB, items: SHARP_CACHE_ITEMS, files: 0});
	const sharpConcurrency = sharp.concurrency(Math.min(sharp.concurrency(), resources.cores));
	logger.info(
		{
			cores: resources.cores,
			memoryMb: Math.round(resources.memoryBytes / MIB),
			heapLimitMb: Math.round(getHeapStatistics().heap_size_limit / MIB),
			sharpConcurrency,
			sharpCacheMb: SHARP_CACHE_MEMORY_MB,
		},
		'Sized the process for the host',
	);
	return resources;
}
