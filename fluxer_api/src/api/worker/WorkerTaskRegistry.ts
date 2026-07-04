// SPDX-License-Identifier: AGPL-3.0-or-later

import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import applicationProcessDeletion from './tasks/ApplicationProcessDeletion';
import bulkAddGuildMembers from './tasks/admin_bulk/BulkAddGuildMembers';
import bulkBanFileShas from './tasks/admin_bulk/BulkBanFileShas';
import bulkScheduleUserDeletion from './tasks/admin_bulk/BulkScheduleUserDeletion';
import bulkUpdateGuildFeatures from './tasks/admin_bulk/BulkUpdateGuildFeatures';
import bulkUpdateSuspiciousActivityFlags from './tasks/admin_bulk/BulkUpdateSuspiciousActivityFlags';
import bulkUpdateUserFlags from './tasks/admin_bulk/BulkUpdateUserFlags';
import batchGuildAuditLogMessageDeletes from './tasks/BatchGuildAuditLogMessageDeletes';
import bulkDeleteSelfMessagesImmediate from './tasks/BulkDeleteSelfMessagesImmediate';
import bulkDeleteUserMessages from './tasks/BulkDeleteUserMessages';
import bulkDeleteUserMessagesScoped from './tasks/BulkDeleteUserMessagesScoped';
import deleteUserMessagesInGuildByTime from './tasks/DeleteUserMessagesInGuildByTime';
import expireAttachments from './tasks/ExpireAttachments';
import extractEmbeds from './tasks/ExtractEmbeds';
import finalizeNcmecAttachmentReport from './tasks/FinalizeNcmecAttachmentReport';
import flushUserActivityBuffer from './tasks/FlushUserActivityBuffer';
import handleMentionChunk from './tasks/HandleMentionChunk';
import handleMentions from './tasks/HandleMentions';
import harvestGuildData from './tasks/HarvestGuildData';
import harvestUserData from './tasks/HarvestUserData';
import indexChannelMessages from './tasks/IndexChannelMessages';
import indexGuildMembers from './tasks/IndexGuildMembers';
import messageShred from './tasks/MessageShred';
import processAssetDeletionQueue from './tasks/ProcessAssetDeletionQueue';
import processBunnyPurgeQueue from './tasks/ProcessBunnyPurgeQueue';
import processExpiredPremiumSweep from './tasks/ProcessExpiredPremiumSweep';
import processInactivityDeletions from './tasks/ProcessInactivityDeletions';
import processPendingBulkMessageDeletions from './tasks/ProcessPendingBulkMessageDeletions';
import processPremiumStateReconciliationQueue from './tasks/ProcessPremiumStateReconciliationQueue';
import processStripeWebhook from './tasks/ProcessStripeWebhook';
import prunePostgresKvTtl from './tasks/PrunePostgresKvTtl';
import reconcileUserPayments from './tasks/ReconcileUserPayments';
import refreshSearchIndex from './tasks/RefreshSearchIndex';
import revalidateUserConnections from './tasks/RevalidateUserConnections';
import {sendScheduledMessage} from './tasks/SendScheduledMessage';
import {sendSystemDm} from './tasks/SendSystemDm';
import syncDiscoveryIndex from './tasks/SyncDiscoveryIndex';
import syncDisposableEmailDomains from './tasks/SyncDisposableEmailDomains';
import syncFileShaBlocklists from './tasks/SyncFileShaBlocklists';
import syncUrlBlocklists from './tasks/SyncUrlBlocklists';
import userProcessPendingDeletion from './tasks/UserProcessPendingDeletion';
import userProcessPendingDeletions from './tasks/UserProcessPendingDeletions';
import type {WorkerTaskName} from './WorkerLaneConfig';

export const workerTasks: Record<WorkerTaskName, WorkerTaskHandler> = {
	applicationProcessDeletion,
	batchGuildAuditLogMessageDeletes,
	bulkAddGuildMembers: bulkAddGuildMembers,
	bulkBanFileShas: bulkBanFileShas,
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
	processBunnyPurgeQueue,
	processStripeWebhook,
	processExpiredPremiumSweep,
	processInactivityDeletions,
	processPendingBulkMessageDeletions,
	processPremiumStateReconciliationQueue,
	reconcileUserPayments,
	prunePostgresKvTtl,
	refreshSearchIndex,
	revalidateUserConnections,
	sendScheduledMessage,
	sendSystemDm,
	syncFileShaBlocklists,
	syncUrlBlocklists,
	syncDiscoveryIndex,
	syncDisposableEmailDomains,
	flushUserActivityBuffer,
	userProcessPendingDeletion,
	userProcessPendingDeletions,
};
