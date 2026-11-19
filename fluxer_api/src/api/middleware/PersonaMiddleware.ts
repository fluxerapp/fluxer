import { createMiddleware } from "hono/factory";
import type { HonoEnv } from "../types/HonoEnv";
import { PersonaService } from "../personas/services/PersonaService";
import { PersonaRepository } from "../personas/repositories/PersonaRepository";
import { AttachmentUploadTraceRepository } from "../channel/repositories/message/AttachmentUploadTraceRepository";
import { VirusScanService } from "../infrastructure/VirusScanService";
import { GuildRepository } from "../guild/repositories/GuildRepository";
import { UserAccountRepository } from "../user/repositories/account/UserAccountRepository";
import { getKVClient } from "./ServiceRegistry";
import { UserAccountService } from "../user/services/UserAccountService";

export const PersonaMiddleware = createMiddleware<HonoEnv>(async (ctx, next) => {
	ctx.set("personaService", new PersonaService(
		new PersonaRepository(),
		ctx.get("snowflakeService"),
		ctx.get("entityAssetService"),
		ctx.get("userCacheService"),
		ctx.get("requestCache"),
		ctx.get("storageService"),
		new AttachmentUploadTraceRepository(),
		ctx.get("mediaService"),
		new VirusScanService(ctx.get("cacheService")),
		ctx.get("rateLimitService"),
		ctx.get("limitConfigService"),
		new UserAccountRepository(getKVClient()),
		new GuildRepository(ctx.get("requestCache")),
	));
	return next();
});
