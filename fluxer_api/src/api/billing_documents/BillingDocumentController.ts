// SPDX-License-Identifier: AGPL-3.0-or-later

import {billingDocumentPdfResponse, mapBillingDocument} from '@app/api/billing_documents/BillingDocumentMappers';
import {BillingDocumentRepository} from '@app/api/billing_documents/BillingDocumentRepository';
import {DefaultUserOnly, LoginRequired} from '@app/api/middleware/AuthMiddleware';
import {HostedOnlyRoute} from '@app/api/middleware/BillingRouteAvailability';
import {RateLimitMiddleware} from '@app/api/middleware/RateLimitMiddleware';
import {OpenAPI} from '@app/api/middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '@app/api/RateLimitConfig';
import type {HonoApp} from '@app/api/types/HonoEnv';
import {Validator} from '@app/api/Validator';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {NotFoundError} from '@fluxer/errors/src/domains/core/NotFoundError';
import {
	BillingDocumentFileResponse,
	BillingDocumentIdParam,
	BillingDocumentListQuery,
	BillingDocumentListResponse,
} from '@fluxer/schema/src/domains/billing_documents/BillingDocumentSchemas';

const TAG = 'Billing';

export function BillingDocumentController(app: HonoApp) {
	const repository = new BillingDocumentRepository();
	app.get(
		'/users/@me/billing/documents',
		HostedOnlyRoute,
		RateLimitMiddleware(RateLimitConfigs.GIFTS_LIST),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'list_billing_documents',
			summary: 'List billing documents',
			description:
				'Lists the invoices, receipts, credit notes and refund notes Fluxer issued to the current account, newest first. Each one can be downloaded as the PDF that was issued.',
			responseSchema: BillingDocumentListResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: TAG,
		}),
		Validator('query', BillingDocumentListQuery),
		async (ctx) => {
			const {before, limit} = ctx.req.valid('query');
			const rows = await repository.listUserDocuments(ctx.get('user').id, before ? new Date(before) : null, limit);
			return ctx.json(rows.map(mapBillingDocument));
		},
	);
	app.get(
		'/users/@me/billing/documents/:document_id/pdf',
		HostedOnlyRoute,
		RateLimitMiddleware(RateLimitConfigs.GIFTS_LIST),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'download_billing_document',
			summary: 'Download a billing document',
			description:
				'Returns the stored PDF of one billing document issued to the current account, byte for byte as it was issued. A document of another account returns 404.',
			responseSchema: BillingDocumentFileResponse,
			responseContentType: 'application/pdf',
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: TAG,
		}),
		Validator('param', BillingDocumentIdParam),
		async (ctx) => {
			const row = await repository.findDocument(ctx.req.valid('param').document_id);
			if (!row || row.user_id !== ctx.get('user').id) {
				throw new NotFoundError({code: APIErrorCodes.NOT_FOUND});
			}
			return billingDocumentPdfResponse(row);
		},
	);
}
