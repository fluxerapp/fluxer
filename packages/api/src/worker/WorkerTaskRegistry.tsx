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

import applicationProcessDeletion from '@fluxer/api/src/worker/tasks/ApplicationProcessDeletion';
import batchGuildAuditLogMessageDeletes from '@fluxer/api/src/worker/tasks/BatchGuildAuditLogMessageDeletes';
import bulkDeleteUserMessages from '@fluxer/api/src/worker/tasks/BulkDeleteUserMessages';
// import cleanupCsamEvidence from '@fluxer/api/src/worker/tasks/CleanupCsamEvidence';
// import csamScanConsumer from '@fluxer/api/src/worker/tasks/CsamScanConsumerWorker';
import deleteUserMessagesInGuildByTime from '@fluxer/api/src/worker/tasks/DeleteUserMessagesInGuildByTime';
import expireAttachments from '@fluxer/api/src/worker/tasks/ExpireAttachments';
import extractEmbeds from '@fluxer/api/src/worker/tasks/ExtractEmbeds';
import handleMentions from '@fluxer/api/src/worker/tasks/HandleMentions';
import harvestGuildData from '@fluxer/api/src/worker/tasks/HarvestGuildData';
import harvestUserData from '@fluxer/api/src/worker/tasks/HarvestUserData';
import indexChannelMessages from '@fluxer/api/src/worker/tasks/IndexChannelMessages';
import indexGuildMembers from '@fluxer/api/src/worker/tasks/IndexGuildMembers';
import messageShred from '@fluxer/api/src/worker/tasks/MessageShred';
import processAssetDeletionQueue from '@fluxer/api/src/worker/tasks/ProcessAssetDeletionQueue';
import processCloudflarePurgeQueue from '@fluxer/api/src/worker/tasks/ProcessCloudflarePurgeQueue';
import processInactivityDeletions from '@fluxer/api/src/worker/tasks/ProcessInactivityDeletions';
import processPendingBulkMessageDeletions from '@fluxer/api/src/worker/tasks/ProcessPendingBulkMessageDeletions';
import refreshSearchIndex from '@fluxer/api/src/worker/tasks/RefreshSearchIndex';
import revalidateUserConnections from '@fluxer/api/src/worker/tasks/RevalidateUserConnections';
import {sendScheduledMessage} from '@fluxer/api/src/worker/tasks/SendScheduledMessage';
import {sendSystemDm} from '@fluxer/api/src/worker/tasks/SendSystemDm';
import syncDiscoveryIndex from '@fluxer/api/src/worker/tasks/SyncDiscoveryIndex';
import userProcessPendingDeletion from '@fluxer/api/src/worker/tasks/UserProcessPendingDeletion';
import userProcessPendingDeletions from '@fluxer/api/src/worker/tasks/UserProcessPendingDeletions';
import type {WorkerTaskHandler} from '@fluxer/worker/src/contracts/WorkerTask';

export const workerTasks: Record<string, WorkerTaskHandler> = {
	applicationProcessDeletion,
	batchGuildAuditLogMessageDeletes,
	bulkDeleteUserMessages,
	// csamScanConsumer,
	deleteUserMessagesInGuildByTime,
	expireAttachments,
	extractEmbeds,
	handleMentions,
	harvestGuildData,
	harvestUserData,
	indexChannelMessages,
	indexGuildMembers,
	messageShred,
	processAssetDeletionQueue,
	// cleanupCsamEvidence,
	processCloudflarePurgeQueue,
	processInactivityDeletions,
	processPendingBulkMessageDeletions,
	refreshSearchIndex,
	revalidateUserConnections,
	sendScheduledMessage,
	sendSystemDm,
	syncDiscoveryIndex,
	userProcessPendingDeletion,
	userProcessPendingDeletions,
};
