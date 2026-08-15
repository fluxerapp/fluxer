// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {
    PendingRegistrationsActionResponse,
    PendingRegistrationActionRequest,
} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import type {Context} from 'hono';
import {createMiddleware} from 'hono/factory';
import {createUserID} from '../../BrandedTypes';
import {
    REGISTRATION_PENDING_APPROVAL_TRAIT,
    REGISTRATION_REJECTED_TRAIT,
} from '../../instance/InstanceConfigRepository';
import {requireAdminACL} from '../../middleware/AdminMiddleware';
import {RateLimitMiddleware} from '../../middleware/RateLimitMiddleware';
import {OpenAPI} from '../../middleware/ResponseTypeMiddleware';
import {getInstanceConfigRepository} from '../../middleware/ServiceSingletons';
import {RateLimitConfigs} from '../../RateLimitConfig';
import type {HonoApp, HonoEnv} from '../../types/HonoEnv';
import {Validator} from '../../Validator';

async function buildRegistrationResponse(): Promise<PendingRegistrationsActionResponse> {
    const pending_registrations = await getInstanceConfigRepository().getPendingRegistrations();
    return pending_registrations
}

function requireSetupSessionOrAdminACL(requiredACL: string) {
    const requireAcl = requireAdminACL(requiredACL);
    return createMiddleware<HonoEnv>(async (ctx, next) => {
        const user = ctx.get('user');
        const tokenType = ctx.get('authTokenType');
        if (user && tokenType === 'session') {
            const appPublic = await getInstanceConfigRepository().getAppPublicConfig();
            if (!appPublic.setup.configured) {
                ctx.set('adminUserId', user.id);
                ctx.set('adminUserAcls', user.acls);
                await next();
                return;
            }
        }
        return requireAcl(ctx, next);
    });
}

export function PendingRegistrationsAdminController(app: HonoApp) {
    const instanceConfigRepository = getInstanceConfigRepository();
    app.post(
        '/admin/pending-registrations/get',
        RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
        requireSetupSessionOrAdminACL(AdminACLs.USER_APPROVE_ACCOUNT),
        OpenAPI({
            operationId: 'get_pending_registrations',
            summary: 'Get pending user registrations',
            description:
                'Retrieves pending user registrations in in the form of an array. Requires USER_APPROVE_ACCOUNT permission.',
            responseSchema: PendingRegistrationsActionResponse,
            statusCode: 200,
            security: 'adminApiKey',
            tags: 'Admin',
        }),
        async (ctx) => {
            return ctx.json(await buildRegistrationResponse());
        },
    );
    app.post(
        '/admin/pending-registrations/approve',
        RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
        requireAdminACL(AdminACLs.USER_APPROVE_ACCOUNT),
        Validator('json', PendingRegistrationActionRequest),
        OpenAPI({
            operationId: 'approve_pending_registration',
            summary: 'Approve a pending registration',
            description:
                'Approves a registration waiting for manual review by removing its pending registration trait. Requires USER_APPROVE_ACCOUNT permission.',
            responseSchema: PendingRegistrationsActionResponse,
            statusCode: 200,
            security: 'adminApiKey',
            tags: 'Admin',
        }),
        async (ctx) => {
            const userId = ctx.req.valid('json').user_id;
            await updatePendingRegistrationUser(ctx, userId, 'approve');
            await instanceConfigRepository.removePendingRegistration(userId);
            return ctx.json(await buildRegistrationResponse());
        },
    );
    app.post(
        '/admin/pending-registrations/reject',
        RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
        requireAdminACL(AdminACLs.USER_APPROVE_ACCOUNT),
        Validator('json', PendingRegistrationActionRequest),
        OpenAPI({
            operationId: 'reject_pending_registration',
            summary: 'Reject a pending registration',
            description:
                'Rejects a registration waiting for manual review and prevents the account from logging in. Requires USER_APPROVE_ACCOUNT permission.',
            responseSchema: PendingRegistrationsActionResponse,
            statusCode: 200,
            security: 'adminApiKey',
            tags: 'Admin',
        }),
        async (ctx) => {
            const userId = ctx.req.valid('json').user_id;
            await updatePendingRegistrationUser(ctx, userId, 'reject');
            await instanceConfigRepository.removePendingRegistration(userId);
            return ctx.json(await buildRegistrationResponse());
        },
    );
}

async function updatePendingRegistrationUser(
    ctx: Context<HonoEnv>,
    userId: string,
    decision: 'approve' | 'reject',
): Promise<void> {
    const userRepository = ctx.get('userRepository');
    const user = await userRepository.findUnique(createUserID(BigInt(userId)));
    if (!user) {
        return;
    }
    const traits = new Set(user.traits);
    traits.delete(REGISTRATION_PENDING_APPROVAL_TRAIT);
    if (decision === 'reject') {
        traits.add(REGISTRATION_REJECTED_TRAIT);
    } else {
        traits.delete(REGISTRATION_REJECTED_TRAIT);
    }
    await userRepository.patchUpsert(user.id, {traits: traits.size > 0 ? traits : null}, user.toRow());
    await ctx.get('adminService').auditService.createAuditLog({
        adminUserId: ctx.get('adminUserId'),
        targetType: 'user',
        targetId: user.id,
        action: decision === 'approve' ? 'approve_registration' : 'reject_registration',
        auditLogReason: ctx.get('auditLogReason'),
    });
}
