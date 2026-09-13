// SPDX-License-Identifier: AGPL-3.0-or-later

import applicationProcessDeletion from '@app/api/worker/tasks/ApplicationProcessDeletion';
import bulkAddGuildMembers from '@app/api/worker/tasks/admin_bulk/BulkAddGuildMembers';
import bulkBanFileShas from '@app/api/worker/tasks/admin_bulk/BulkBanFileShas';
import bulkDeleteMessagesForUsers from '@app/api/worker/tasks/admin_bulk/BulkDeleteMessagesForUsers';
import bulkScheduleUserDeletion from '@app/api/worker/tasks/admin_bulk/BulkScheduleUserDeletion';
import bulkUpdateGuildFeatures from '@app/api/worker/tasks/admin_bulk/BulkUpdateGuildFeatures';
import bulkUpdateSuspiciousActivityFlags from '@app/api/worker/tasks/admin_bulk/BulkUpdateSuspiciousActivityFlags';
import bulkUpdateUserFlags from '@app/api/worker/tasks/admin_bulk/BulkUpdateUserFlags';
import batchGuildAuditLogMessageDeletes from '@app/api/worker/tasks/BatchGuildAuditLogMessageDeletes';
import bulkDeleteSelfMessagesImmediate from '@app/api/worker/tasks/BulkDeleteSelfMessagesImmediate';
import bulkDeleteUserMessages from '@app/api/worker/tasks/BulkDeleteUserMessages';
import bulkDeleteUserMessagesScoped from '@app/api/worker/tasks/BulkDeleteUserMessagesScoped';
import deleteUserMessagesInGuildByTime from '@app/api/worker/tasks/DeleteUserMessagesInGuildByTime';
import expireAttachments from '@app/api/worker/tasks/ExpireAttachments';
import extractEmbeds from '@app/api/worker/tasks/ExtractEmbeds';
import finalizeNcmecAttachmentReport from '@app/api/worker/tasks/FinalizeNcmecAttachmentReport';
import flushUserActivityBuffer from '@app/api/worker/tasks/FlushUserActivityBuffer';
import handleMentionChunk from '@app/api/worker/tasks/HandleMentionChunk';
import handleMentions from '@app/api/worker/tasks/HandleMentions';
import harvestGuildData from '@app/api/worker/tasks/HarvestGuildData';
import harvestUserData from '@app/api/worker/tasks/HarvestUserData';
import indexChannelMessages from '@app/api/worker/tasks/IndexChannelMessages';
import indexGuildMembers from '@app/api/worker/tasks/IndexGuildMembers';
import messageShred from '@app/api/worker/tasks/MessageShred';
import processAssetDeletionQueue from '@app/api/worker/tasks/ProcessAssetDeletionQueue';
import processCachePurgeQueue from '@app/api/worker/tasks/ProcessCachePurgeQueue';
import processExpiredPremiumSweep from '@app/api/worker/tasks/ProcessExpiredPremiumSweep';
import processInactivityDeletions from '@app/api/worker/tasks/ProcessInactivityDeletions';
import processPendingBulkMessageDeletions from '@app/api/worker/tasks/ProcessPendingBulkMessageDeletions';
import processPremiumStateReconciliationQueue from '@app/api/worker/tasks/ProcessPremiumStateReconciliationQueue';
import processStripeWebhook from '@app/api/worker/tasks/ProcessStripeWebhook';
import prunePostgresKvTtl from '@app/api/worker/tasks/PrunePostgresKvTtl';
import reconcileUserPayments from '@app/api/worker/tasks/ReconcileUserPayments';
import refreshSearchIndex from '@app/api/worker/tasks/RefreshSearchIndex';
import {sendSystemDm} from '@app/api/worker/tasks/SendSystemDm';
import syncDiscoveryIndex from '@app/api/worker/tasks/SyncDiscoveryIndex';
import syncDisposableEmailDomains from '@app/api/worker/tasks/SyncDisposableEmailDomains';
import syncFileShaBlocklists from '@app/api/worker/tasks/SyncFileShaBlocklists';
import syncUrlBlocklists from '@app/api/worker/tasks/SyncUrlBlocklists';
import userProcessPendingDeletion from '@app/api/worker/tasks/UserProcessPendingDeletion';
import userProcessPendingDeletions from '@app/api/worker/tasks/UserProcessPendingDeletions';
import type {WorkerTaskName} from '@app/api/worker/WorkerLaneConfig';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';

export const workerTasks: Record<WorkerTaskName, WorkerTaskHandler> = {
	applicationProcessDeletion,
	batchGuildAuditLogMessageDeletes,
	bulkAddGuildMembers: bulkAddGuildMembers,
	bulkBanFileShas: bulkBanFileShas,
	bulkDeleteMessagesForUsers: bulkDeleteMessagesForUsers,
	bulkDeleteSelfMessagesImmediate,
	bulkDeleteUserMessages,
	bulkDeleteUserMessagesScoped,
	bulkScheduleUserDeletion: bulkScheduleUserDeletion,
	bulkUpdateGuildFeatures: bulkUpdateGuildFeatures,
	bulkUpdateSuspiciousActivityFlags: bulkUpdateSuspiciousActivityFlags,
	bulkUpdateUserFlags: bulkUpdateUserFlags,
	deleteUserMessagesInGuildByTime,
	expireAttachments,
	extractEmbeds,
	finalizeNcmecAttachmentReport,
	handleMentions,
	handleMentionChunk,
	harvestGuildData,
	harvestUserData,
	indexChannelMessages,
	indexGuildMembers,
	messageShred,
	processAssetDeletionQueue,
	processCachePurgeQueue,
	processStripeWebhook,
	processExpiredPremiumSweep,
	processInactivityDeletions,
	processPendingBulkMessageDeletions,
	processPremiumStateReconciliationQueue,
	reconcileUserPayments,
	prunePostgresKvTtl,
	refreshSearchIndex,
	sendSystemDm,
	syncFileShaBlocklists,
	syncUrlBlocklists,
	syncDiscoveryIndex,
	syncDisposableEmailDomains,
	flushUserActivityBuffer,
	userProcessPendingDeletion,
	userProcessPendingDeletions,
};
