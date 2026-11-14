// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	transformCheckRun,
	transformCheckSuite,
	transformDiscussion,
	transformDiscussionComment,
} from '@app/api/webhook/transformers/GitHubCheckTransformer';
import {
	transformCommitComment,
	transformCreate,
	transformDelete,
	transformPush,
} from '@app/api/webhook/transformers/GitHubCommitTransformer';
import {transformIssue, transformIssueComment} from '@app/api/webhook/transformers/GitHubIssueTransformer';
import {
	transformPullRequest,
	transformPullRequestReview,
	transformPullRequestReviewComment,
} from '@app/api/webhook/transformers/GitHubPullRequestTransformer';
import {
	transformFork,
	transformMember,
	transformPublic,
	transformRelease,
	transformRepository,
	transformWatch,
} from '@app/api/webhook/transformers/GitHubRepositoryTransformer';
import type {RichEmbedRequest} from '@fluxer/schema/src/domains/message/MessageRequestSchemas';
import type {GitHubWebhook} from '@fluxer/schema/src/domains/webhook/GitHubWebhookSchemas';

export async function transform(event: string, body: GitHubWebhook): Promise<RichEmbedRequest | null> {
	switch (event) {
		case 'commit_comment':
			return transformCommitComment(body);
		case 'create':
			return transformCreate(body);
		case 'delete':
			return transformDelete(body);
		case 'fork':
			return transformFork(body);
		case 'issue_comment':
			return transformIssueComment(body);
		case 'issues':
			return transformIssue(body);
		case 'member':
			return transformMember(body);
		case 'public':
			return transformPublic(body);
		case 'pull_request':
			return transformPullRequest(body);
		case 'pull_request_review':
			return transformPullRequestReview(body);
		case 'pull_request_review_comment':
			return transformPullRequestReviewComment(body);
		case 'push':
			return transformPush(body);
		case 'release':
			return transformRelease(body);
		case 'watch':
			return transformWatch(body);
		case 'check_run':
			return transformCheckRun(body);
		case 'check_suite':
			return transformCheckSuite(body);
		case 'discussion':
			return transformDiscussion(body);
		case 'discussion_comment':
			return transformDiscussionComment(body);
		case 'repository':
			return transformRepository(body);
		default:
			return null;
	}
}
