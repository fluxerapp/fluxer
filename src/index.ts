/**
 * Fluxer Message Threads Architecture
 * Public API & Subsystem Exports
 */

export * from './threads/types';
export * from './threads/events';
export * from './threads/notifications';
export * from './threads/messages';
export * from './threads/manager';

import { ThreadManager, ThreadManagerOptions } from './threads/manager';
import { ThreadNotificationEngine, UserRoleResolver } from './threads/notifications';
import { ThreadMessageService } from './threads/messages';

/**
 * Convenience factory to create a fully wired Fluxer Threads subsystem.
 */
export function createFluxerThreadsSystem(options: {
  roleResolver?: UserRoleResolver;
  permissionChecker?: ThreadManagerOptions['permissionChecker'];
} = {}): {
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
