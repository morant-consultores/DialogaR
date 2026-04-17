#!/usr/bin/env bash
# =============================================================================
# install_hooks.sh — Instala hooks de git para cumplimiento ISO 27001
#
# Uso (desde la raíz del repositorio):
#   bash inst/hooks/install_hooks.sh
#
# Los hooks se copian a .git/hooks/ y se hacen ejecutables.
# Los archivos fuente viven en inst/hooks/ (versionados con el paquete).
# =============================================================================

set -euo pipefail

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RESET='\033[0m'

# ── Verificar que estamos en la raíz del repositorio ─────────────────────────

if [[ ! -d ".git" ]]; then
  echo -e "${RED}Error: ejecutar desde la raíz del repositorio (donde está .git/)${RESET}"
  exit 1
fi

if [[ ! -d "inst/hooks" ]]; then
  echo -e "${RED}Error: no se encontró inst/hooks/. ¿Estás en el directorio correcto?${RESET}"
  exit 1
fi

GIT_HOOKS_DIR="$(git rev-parse --git-dir)/hooks"
SOURCE_DIR="inst/hooks"

# ── Instalar cada hook ────────────────────────────────────────────────────────

HOOKS=("pre-commit" "commit-msg")

for hook in "${HOOKS[@]}"; do
  SOURCE="$SOURCE_DIR/$hook"
  TARGET="$GIT_HOOKS_DIR/$hook"

  if [[ ! -f "$SOURCE" ]]; then
    echo -e "${YELLOW}⚠️  No encontrado: $SOURCE — omitiendo${RESET}"
    continue
  fi

  # Hacer backup si ya existe un hook personalizado
  if [[ -f "$TARGET" ]] && ! diff -q "$SOURCE" "$TARGET" > /dev/null 2>&1; then
    BACKUP="$TARGET.backup.$(date +%Y%m%d_%H%M%S)"
    cp "$TARGET" "$BACKUP"
    echo -e "${YELLOW}⚠️  Hook existente respaldado en: $BACKUP${RESET}"
  fi

  cp "$SOURCE" "$TARGET"
  chmod +x "$TARGET"
  echo -e "${GREEN}✅ Instalado: $TARGET${RESET}"
done

# ── Verificar bash disponible (requerido por los hooks) ──────────────────────

if ! command -v bash &> /dev/null; then
  echo -e "${YELLOW}⚠️  bash no encontrado en PATH. Los hooks pueden no funcionar.${RESET}"
fi

# ── Resumen ───────────────────────────────────────────────────────────────────

echo ""
echo "Hooks instalados en: $GIT_HOOKS_DIR"
echo ""
echo "  pre-commit  — valida antes de cada commit:"
echo "    • Archivos sensibles (.Renviron, *.token, credentials.json)"
echo "    • Encabezado de módulo en archivos R nuevos (campo Datos:)"
echo "    • @section Seguridad y Privacidad en funciones exportadas nuevas"
echo "    • PII expuesta en mensajes de error (stop/message/warning)"
echo "    • Credenciales hardcodeadas"
echo "    • Dependencias nuevas en DESCRIPTION"
echo ""
echo "  commit-msg  — valida el mensaje de cada commit:"
echo "    • Formato Conventional Commits (<tipo>(<alcance>): <descripción>)"
echo "    • Subject line ≤ 72 caracteres"
echo "    • Commits 'security' requieren cuerpo con referencia ISO 27001"
echo ""
echo "Para omitir los hooks en una emergencia (no recomendado):"
echo "  git commit --no-verify"
echo ""
echo "Para desinstalar:"
echo "  rm $GIT_HOOKS_DIR/pre-commit $GIT_HOOKS_DIR/commit-msg"
