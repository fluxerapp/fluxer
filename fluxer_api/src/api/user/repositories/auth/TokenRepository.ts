// SPDX-License-Identifier: AGPL-3.0-or-later

import type {PasswordResetToken as PasswordResetTokenBrand, UserID} from '@app/api/BrandedTypes';
import {createEmailRevertToken, createEmailVerificationToken, createPasswordResetToken} from '@app/api/BrandedTypes';
import {BatchBuilder, deleteOneOrMany, fetchMany, fetchOne, upsertOne} from '@app/api/database/CassandraQueryExecution';
import type {
	EmailRevertTokenRow,
	EmailVerificationTokenRow,
	PasswordResetTokenRow,
} from '@app/api/database/types/AuthTypes';
import {EmailRevertToken} from '@app/api/models/EmailRevertToken';
import {EmailVerificationToken} from '@app/api/models/EmailVerificationToken';
import {PasswordResetToken} from '@app/api/models/PasswordResetToken';
import {
	EmailRevertTokens,
	EmailVerificationTokens,
	PasswordResetTokens,
	PasswordResetTokensByUserId,
} from '@app/api/Tables';
import {seconds} from 'itty-time';

const FETCH_EMAIL_VERIFICATION_TOKEN_CQL = EmailVerificationTokens.selectCql({
	where: EmailVerificationTokens.where.eq('token_'),
	limit: 1,
});
const FETCH_PASSWORD_RESET_TOKEN_CQL = PasswordResetTokens.selectCql({
	where: PasswordResetTokens.where.eq('token_'),
	limit: 1,
});
const FETCH_PASSWORD_RESET_TOKENS_BY_USER_CQL = PasswordResetTokensByUserId.selectCql({
	where: PasswordResetTokensByUserId.where.eq('user_id'),
});
export const PASSWORD_RESET_TOKEN_TTL_SECONDS = seconds('1 hour');

const FETCH_EMAIL_REVERT_TOKEN_CQL = EmailRevertTokens.selectCql({
	where: EmailRevertTokens.where.eq('token_'),
	limit: 1,
});

export class TokenRepository {
	async getEmailVerificationToken(token: string): Promise<EmailVerificationToken | null> {
		const tokenRow = await fetchOne<EmailVerificationTokenRow>(FETCH_EMAIL_VERIFICATION_TOKEN_CQL, {token_: token});
		return tokenRow ? new EmailVerificationToken(tokenRow) : null;
	}

	async createEmailVerificationToken(tokenData: EmailVerificationTokenRow): Promise<EmailVerificationToken> {
		await upsertOne(EmailVerificationTokens.insertWithTtl(tokenData, seconds('24 hours')));
		return new EmailVerificationToken(tokenData);
	}

	async deleteEmailVerificationToken(token: string): Promise<void> {
		await deleteOneOrMany(
			EmailVerificationTokens.deleteCql({
				where: EmailVerificationTokens.where.eq('token_'),
			}),
			{token_: createEmailVerificationToken(token)},
		);
	}

	async getPasswordResetToken(token: string): Promise<PasswordResetToken | null> {
		const tokenRow = await fetchOne<PasswordResetTokenRow>(FETCH_PASSWORD_RESET_TOKEN_CQL, {token_: token});
		return tokenRow ? new PasswordResetToken(tokenRow) : null;
	}

	async createPasswordResetToken(tokenData: PasswordResetTokenRow): Promise<PasswordResetToken> {
		const batch = new BatchBuilder();
		batch.addPrepared(PasswordResetTokens.insertWithTtl(tokenData, PASSWORD_RESET_TOKEN_TTL_SECONDS));
		batch.addPrepared(
			PasswordResetTokensByUserId.insertWithTtl(
				{user_id: tokenData.user_id, token_: tokenData.token_},
				PASSWORD_RESET_TOKEN_TTL_SECONDS,
			),
		);
		await batch.execute();
		return new PasswordResetToken(tokenData);
	}

	async deletePasswordResetToken(token: string): Promise<void> {
		const brandedToken = createPasswordResetToken(token);
		const tokenRow = await this.getPasswordResetToken(token);
		if (tokenRow) {
			const batch = new BatchBuilder();
			batch.addPrepared(PasswordResetTokens.deleteByPk({token_: brandedToken, user_id: tokenRow.userId}));
			batch.addPrepared(PasswordResetTokensByUserId.deleteByPk({user_id: tokenRow.userId, token_: brandedToken}));
			await batch.execute();
		} else {
			await deleteOneOrMany(PasswordResetTokens.deleteCql({where: PasswordResetTokens.where.eq('token_')}), {
				token_: brandedToken,
			});
		}
	}

	async deleteAllPasswordResetTokens(userId: UserID): Promise<void> {
		const tokens = await fetchMany<{
			user_id: UserID;
			token_: PasswordResetTokenBrand;
		}>(FETCH_PASSWORD_RESET_TOKENS_BY_USER_CQL, {
			user_id: userId,
		});
		if (tokens.length === 0) return;
		const batch = new BatchBuilder();
		for (const token of tokens) {
			batch.addPrepared(PasswordResetTokens.deleteByPk({token_: token.token_, user_id: userId}));
			batch.addPrepared(PasswordResetTokensByUserId.deleteByPk({user_id: userId, token_: token.token_}));
		}
		await batch.execute();
	}

	async getEmailRevertToken(token: string): Promise<EmailRevertToken | null> {
		const tokenRow = await fetchOne<EmailRevertTokenRow>(FETCH_EMAIL_REVERT_TOKEN_CQL, {token_: token});
		return tokenRow ? new EmailRevertToken(tokenRow) : null;
	}

	async createEmailRevertToken(tokenData: EmailRevertTokenRow): Promise<EmailRevertToken> {
		await upsertOne(EmailRevertTokens.insertWithTtl(tokenData, seconds('24 hours')));
		return new EmailRevertToken(tokenData);
	}

	async deleteEmailRevertToken(token: string): Promise<void> {
		await deleteOneOrMany(
			EmailRevertTokens.deleteCql({
				where: EmailRevertTokens.where.eq('token_'),
			}),
			{token_: createEmailRevertToken(token)},
		);
	}
}
