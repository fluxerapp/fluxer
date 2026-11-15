// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {getOperatorPassWallService} from '@app/api/middleware/ServiceSingletons';
import type {HonoApp} from '@app/api/types/HonoEnv';
import {constantTimeEquals} from '@app/api/utils/SecretHash';
import {UnauthorizedError} from '@fluxer/errors/src/domains/core/UnauthorizedError';

const OPERATOR_PASS_WALL_SNAPSHOT_BUCKET = 'operator_pass:wall_snapshot';
const INTERNAL_KEY_HEADER = 'x-fluxer-internal-key';

function isOperatorPassWallSnapshotCaller(providedKey: string): boolean {
	if (providedKey.length === 0) return false;
	let trusted = false;
	for (const caller of Config.internal.trustedCallers) {
		if (!caller.buckets.includes(OPERATOR_PASS_WALL_SNAPSHOT_BUCKET)) continue;
		if (constantTimeEquals(caller.key, providedKey)) trusted = true;
	}
	return trusted;
}

export function OperatorPassInternalController(app: HonoApp): void {
	app.get('/internal/operator-pass/wall', async (ctx) => {
		if (Config.instance.selfHosted || !isOperatorPassWallSnapshotCaller(ctx.req.header(INTERNAL_KEY_HEADER) ?? '')) {
			throw new UnauthorizedError();
		}
		const snapshot = await getOperatorPassWallService().publication.snapshot();
		ctx.header('Cache-Control', 'no-store');
		return ctx.json(snapshot);
	});
}
