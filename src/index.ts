/**
 * Fluxer Message Threads Architecture
 * Public API & Subsystem Exports
 */

export * from './threads/events';
export * from './threads/manager';
export * from './threads/messages';
export * from './threads/notifications';
export * from './threads/types';

import {ThreadManager, type ThreadManagerOptions} from './threads/manager';
import {ThreadMessageService} from './threads/messages';
import {ThreadNotificationEngine, type UserRoleResolver} from './threads/notifications';

/**
 * Convenience factory to create a fully wired Fluxer Threads subsystem.
 */
export function createFluxerThreadsSystem(
	options: {roleResolver?: UserRoleResolver; permissionChecker?: ThreadManagerOptions['permissionChecker']} = {},
): {
	manager: ThreadManager;
	messageService: ThreadMessageService;
	notificationEngine: ThreadNotificationEngine;
} {
	const notificationEngine = new ThreadNotificationEngine(options.roleResolver);
	const messageService = new ThreadMessageService(notificationEngine);
	const manager = new ThreadManager({
		notificationEngine,
		messageService,
		permissionChecker: options.permissionChecker,
	});

	return {
		manager,
		messageService,
		notificationEngine,
	};
}
