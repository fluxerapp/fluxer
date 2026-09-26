// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AttachmentID, UserID} from '@app/api/BrandedTypes';
import {fetchOne, upsertOne} from '@app/api/database/CassandraQueryExecution';
import type {NcmecAttachmentSubmissionRow, NcmecUserWorkflowRow} from '@app/api/database/types/CsamTypes';
import {NcmecAttachmentSubmissions, NcmecUserWorkflows} from '@app/api/Tables';

const GET_ATTACHMENT_SUBMISSION_QUERY = NcmecAttachmentSubmissions.select({
	where: NcmecAttachmentSubmissions.where.eq('attachment_id'),
	limit: 1,
});
const GET_USER_WORKFLOW_QUERY = NcmecUserWorkflows.select({
	where: NcmecUserWorkflows.where.eq('user_id'),
	limit: 1,
});

export class NcmecRepository {
	async getAttachmentSubmission(attachmentId: AttachmentID): Promise<NcmecAttachmentSubmissionRow | null> {
		return await fetchOne<NcmecAttachmentSubmissionRow>(
			GET_ATTACHMENT_SUBMISSION_QUERY.bind({attachment_id: BigInt(attachmentId)}),
		);
	}

	async upsertAttachmentSubmission(row: NcmecAttachmentSubmissionRow): Promise<void> {
		await upsertOne(NcmecAttachmentSubmissions.insert(row));
	}

	async getUserWorkflow(userId: UserID): Promise<NcmecUserWorkflowRow | null> {
		return await fetchOne<NcmecUserWorkflowRow>(GET_USER_WORKFLOW_QUERY.bind({user_id: BigInt(userId)}));
	}

	async upsertUserWorkflow(row: NcmecUserWorkflowRow): Promise<void> {
		await upsertOne(NcmecUserWorkflows.insert(row));
	}
}
