// SPDX-License-Identifier: AGPL-3.0-or-later

import {ThreadCreationService} from '@app/api/channel/services/thread/ThreadCreationService';
import {ThreadDeletionService} from '@app/api/channel/services/thread/ThreadDeletionService';
import {ThreadForumService} from '@app/api/channel/services/thread/ThreadForumService';
import {ThreadListService} from '@app/api/channel/services/thread/ThreadListService';
import {ThreadMemberService} from '@app/api/channel/services/thread/ThreadMemberService';
import {ThreadMemberSettingsService} from '@app/api/channel/services/thread/ThreadMemberSettingsService';
import {ThreadServiceContext, type ThreadServiceDeps} from '@app/api/channel/services/thread/ThreadServiceContext';

export class ThreadService {
	readonly creation: ThreadCreationService;
	readonly members: ThreadMemberService;
	readonly memberSettings: ThreadMemberSettingsService;
	readonly lists: ThreadListService;
	readonly deletion: ThreadDeletionService;
	readonly forum: ThreadForumService;

	constructor(deps: ThreadServiceDeps) {
		const context = new ThreadServiceContext(deps);
		this.creation = new ThreadCreationService(context);
		this.members = new ThreadMemberService(context);
		this.memberSettings = new ThreadMemberSettingsService(context);
		this.lists = new ThreadListService(context);
		this.deletion = new ThreadDeletionService(context);
		this.forum = new ThreadForumService(context);
	}
}
