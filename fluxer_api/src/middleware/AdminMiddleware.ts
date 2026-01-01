/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {createMiddleware} from 'hono/factory';
import type {HonoEnv} from '~/App';
import {AdminACLs} from '~/Constants';
import {MissingACLError, MissingPermissionsError, UnauthorizedError} from '~/Errors';
import {Logger} from '~/Logger';

export const requireAdminACL = (requiredACL: string) =>
	createMiddleware<HonoEnv>(async (ctx, next) => {
		const adminUser = ctx.get('user');
		if (!adminUser) throw new UnauthorizedError();

		const tokenType = ctx.get('authTokenType');
		if (tokenType !== 'bearer' && tokenType !== 'session') throw new UnauthorizedError();

		Logger.debug(
			{
				adminUserId: adminUser.id.toString(),
				acls: Array.from(adminUser.acls),
				requiredACL,
			},
			'Checking admin ACL requirements',
		);
		if (!adminUser.acls.has(AdminACLs.AUTHENTICATE) && !adminUser.acls.has(AdminACLs.WILDCARD)) {
			throw new MissingPermissionsError();
		}

		if (!adminUser.acls.has(requiredACL) && !adminUser.acls.has(AdminACLs.WILDCARD)) {
			throw new MissingACLError(requiredACL);
		}

		ctx.set('adminUserId', adminUser.id);
		ctx.set('adminUserAcls', adminUser.acls);
		await next();
	});

export const requireAnyAdminACL = (requiredACLs: Array<string>) =>
	createMiddleware<HonoEnv>(async (ctx, next) => {
		const adminUser = ctx.get('user');
		if (!adminUser) throw new UnauthorizedError();

		const tokenType = ctx.get('authTokenType');
		if (tokenType !== 'bearer' && tokenType !== 'session') throw new UnauthorizedError();

		Logger.debug(
			{
				adminUserId: adminUser.id.toString(),
				acls: Array.from(adminUser.acls),
				requiredACLs,
			},
			'Checking admin ACL requirements (any)',
		);
		if (!adminUser.acls.has(AdminACLs.AUTHENTICATE) && !adminUser.acls.has(AdminACLs.WILDCARD)) {
			throw new MissingPermissionsError();
		}

		const hasAny = adminUser.acls.has(AdminACLs.WILDCARD) || requiredACLs.some((acl) => adminUser.acls.has(acl));

		if (!hasAny) {
			throw new MissingACLError(requiredACLs[0] ?? AdminACLs.AUTHENTICATE);
		}

		ctx.set('adminUserId', adminUser.id);
		ctx.set('adminUserAcls', adminUser.acls);
		await next();
	});
