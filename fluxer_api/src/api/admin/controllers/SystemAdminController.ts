// SPDX-License-Identifier: AGPL-3.0-or-later

import * as fs from 'node:fs';
import * as path from 'node:path';
import {Readable} from 'node:stream';
import * as v8 from 'node:v8';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {HeapSnapshotResponse} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import {requireAdminACL} from '../../middleware/AdminMiddleware';
import {RateLimitMiddleware} from '../../middleware/RateLimitMiddleware';
import {OpenAPI} from '../../middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '../../RateLimitConfig';
import type {HonoApp} from '../../types/HonoEnv';

export function SystemAdminController(app: HonoApp) {
	app.post(
		'/admin/system/heap-snapshots',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_SYSTEM_HEAP_SNAPSHOT),
		requireAdminACL(AdminACLs.SYSTEM_HEAP_SNAPSHOT),
		OpenAPI({
			operationId: 'create_admin_system_heap_snapshot',
			summary: 'Create a V8 heap snapshot',
			description:
				'Writes a V8 heap snapshot of the current process and returns the snapshot file. Used for diagnosing memory leaks. Requires SYSTEM_HEAP_SNAPSHOT permission.',
			responseSchema: HeapSnapshotResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async () => {
			const snapshotPath = path.join('/tmp', `heap-${Date.now()}.heapsnapshot`);
			try {
				v8.writeHeapSnapshot(snapshotPath);
				const stat = fs.statSync(snapshotPath);
				const nodeStream = fs.createReadStream(snapshotPath);
				const body = Readable.toWeb(nodeStream) as ReadableStream;
				nodeStream.on('close', () => {
					fs.unlink(snapshotPath, () => {});
				});
				return new Response(body, {
					status: 200,
					headers: {
						'Content-Type': 'application/octet-stream',
						'Content-Disposition': `attachment; filename="${path.basename(snapshotPath)}"`,
						'Content-Length': String(stat.size),
					},
				});
			} catch (error) {
				fs.unlink(snapshotPath, () => {});
				throw error;
			}
		},
	);
}
