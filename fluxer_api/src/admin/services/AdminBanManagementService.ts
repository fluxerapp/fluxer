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

import type {IAdminRepository} from '~/admin/IAdminRepository';
import type {UserID} from '~/BrandedTypes';
import {ipBanCache} from '~/middleware/IpBanMiddleware';
import type {AdminAuditService} from './AdminAuditService';

interface AdminBanManagementServiceDeps {
	adminRepository: IAdminRepository;
	auditService: AdminAuditService;
}

export class AdminBanManagementService {
	constructor(private readonly deps: AdminBanManagementServiceDeps) {}

	async banIp(data: {ip: string}, adminUserId: UserID, auditLogReason: string | null) {
		const {adminRepository, auditService} = this.deps;
		await adminRepository.banIp(data.ip);
		ipBanCache.ban(data.ip);

		await auditService.createAuditLog({
			adminUserId,
			targetType: 'ip',
			targetId: BigInt(0),
			action: 'ban_ip',
			auditLogReason,
			metadata: new Map([['ip', data.ip]]),
		});
	}

	async unbanIp(data: {ip: string}, adminUserId: UserID, auditLogReason: string | null) {
		const {adminRepository, auditService} = this.deps;
		await adminRepository.unbanIp(data.ip);
		ipBanCache.unban(data.ip);

		await auditService.createAuditLog({
			adminUserId,
			targetType: 'ip',
			targetId: BigInt(0),
			action: 'unban_ip',
			auditLogReason,
			metadata: new Map([['ip', data.ip]]),
		});
	}

	async checkIpBan(data: {ip: string}): Promise<{banned: boolean}> {
		const banned = ipBanCache.isBanned(data.ip);
		return {banned};
	}

	async banEmail(data: {email: string}, adminUserId: UserID, auditLogReason: string | null) {
		const {adminRepository, auditService} = this.deps;
		await adminRepository.banEmail(data.email);

		await auditService.createAuditLog({
			adminUserId,
			targetType: 'email',
			targetId: BigInt(0),
			action: 'ban_email',
			auditLogReason,
			metadata: new Map([['email', data.email]]),
		});
	}

	async unbanEmail(data: {email: string}, adminUserId: UserID, auditLogReason: string | null) {
		const {adminRepository, auditService} = this.deps;
		await adminRepository.unbanEmail(data.email);

		await auditService.createAuditLog({
			adminUserId,
			targetType: 'email',
			targetId: BigInt(0),
			action: 'unban_email',
			auditLogReason,
			metadata: new Map([['email', data.email]]),
		});
	}

	async checkEmailBan(data: {email: string}): Promise<{banned: boolean}> {
		const {adminRepository} = this.deps;
		const banned = await adminRepository.isEmailBanned(data.email);
		return {banned};
	}

	async banPhone(data: {phone: string}, adminUserId: UserID, auditLogReason: string | null) {
		const {adminRepository, auditService} = this.deps;
		await adminRepository.banPhone(data.phone);

		await auditService.createAuditLog({
			adminUserId,
			targetType: 'phone',
			targetId: BigInt(0),
			action: 'ban_phone',
			auditLogReason,
			metadata: new Map([['phone', data.phone]]),
		});
	}

	async unbanPhone(data: {phone: string}, adminUserId: UserID, auditLogReason: string | null) {
		const {adminRepository, auditService} = this.deps;
		await adminRepository.unbanPhone(data.phone);

		await auditService.createAuditLog({
			adminUserId,
			targetType: 'phone',
			targetId: BigInt(0),
			action: 'unban_phone',
			auditLogReason,
			metadata: new Map([['phone', data.phone]]),
		});
	}

	async checkPhoneBan(data: {phone: string}): Promise<{banned: boolean}> {
		const {adminRepository} = this.deps;
		const banned = await adminRepository.isPhoneBanned(data.phone);
		return {banned};
	}
}
