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

import type {RichEmbedRequest} from '~/channel/ChannelModel';
import {
	transformCheckRun,
	transformCheckSuite,
	transformDiscussion,
	transformDiscussionComment,
} from './GitHubCheckTransformer';
import {transformCommitComment, transformCreate, transformDelete, transformPush} from './GitHubCommitTransformer';
import {transformIssue, transformIssueComment} from './GitHubIssueTransformer';
import {
	transformPullRequest,
	transformPullRequestReview,
	transformPullRequestReviewComment,
} from './GitHubPullRequestTransformer';
import {
	transformFork,
	transformMember,
	transformPublic,
	transformRelease,
	transformRepository,
	transformWatch,
} from './GitHubRepositoryTransformer';
import type {GitHubWebhook} from './GitHubTypes';

export {GitHubWebhook} from './GitHubTypes';

export const transform = async (event: string, body: GitHubWebhook): Promise<RichEmbedRequest | null> => {
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
};
