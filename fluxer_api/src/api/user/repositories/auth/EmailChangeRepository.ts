// SPDX-License-Identifier: AGPL-3.0-or-later

import {deleteOneOrMany, fetchOne, upsertOne} from '@app/api/database/CassandraQueryExecution';
import type {EmailChangeTicketRow, EmailChangeTokenRow} from '@app/api/database/types/AuthTypes';
import {EmailChangeTickets, EmailChangeTokens} from '@app/api/Tables';
import {seconds} from 'itty-time';

const EMAIL_CHANGE_RETENTION_SECONDS = seconds('1 day');

const FETCH_TICKET_CQL = EmailChangeTickets.selectCql({
	where: EmailChangeTickets.where.eq('ticket'),
	limit: 1,
});
const FETCH_TOKEN_CQL = EmailChangeTokens.selectCql({
	where: EmailChangeTokens.where.eq('token_'),
	limit: 1,
});

export class EmailChangeRepository {
	async createTicket(row: EmailChangeTicketRow): Promise<void> {
		await upsertOne(EmailChangeTickets.insertWithTtl(row, EMAIL_CHANGE_RETENTION_SECONDS));
	}

	async updateTicket(row: EmailChangeTicketRow): Promise<void> {
		await upsertOne(EmailChangeTickets.upsertAllWithTtl(row, EMAIL_CHANGE_RETENTION_SECONDS));
	}

	async findTicket(ticket: string): Promise<EmailChangeTicketRow | null> {
		return await fetchOne<EmailChangeTicketRow>(FETCH_TICKET_CQL, {ticket});
	}

	async createToken(row: EmailChangeTokenRow): Promise<void> {
		await upsertOne(EmailChangeTokens.insertWithTtl(row, EMAIL_CHANGE_RETENTION_SECONDS));
	}

	async findToken(token: string): Promise<EmailChangeTokenRow | null> {
		return await fetchOne<EmailChangeTokenRow>(FETCH_TOKEN_CQL, {token_: token});
	}

	async deleteToken(token: string): Promise<void> {
		await deleteOneOrMany(EmailChangeTokens.deleteByPk({token_: token}));
	}
}
