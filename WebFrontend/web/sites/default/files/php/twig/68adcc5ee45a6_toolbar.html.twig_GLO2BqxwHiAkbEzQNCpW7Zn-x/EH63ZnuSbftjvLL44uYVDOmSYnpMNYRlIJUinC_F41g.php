<?php

use Twig\Environment;
use Twig\Error\LoaderError;
use Twig\Error\RuntimeError;
use Twig\Extension\CoreExtension;
use Twig\Extension\SandboxExtension;
use Twig\Markup;
use Twig\Sandbox\SecurityError;
use Twig\Sandbox\SecurityNotAllowedTagError;
use Twig\Sandbox\SecurityNotAllowedFilterError;
use Twig\Sandbox\SecurityNotAllowedFunctionError;
use Twig\Source;
use Twig\Template;
use Twig\TemplateWrapper;

/* core/themes/claro/templates/navigation/toolbar.html.twig */
class __TwigTemplate_1ca17a3082d62797c83af598c2ade242 extends Template
{
    private Source $source;
    /**
     * @var array<string, Template>
     */
    private array $macros = [];

    public function __construct(Environment $env)
    {
        parent::__construct($env);

        $this->source = $this->getSourceContext();

        $this->parent = false;

        $this->blocks = [
        ];
        $this->sandbox = $this->extensions[SandboxExtension::class];
        $this->checkSecurity();
    }

    protected function doDisplay(array $context, array $blocks = []): iterable
    {
        $macros = $this->macros;
        // line 26
        yield "<div";
        yield $this->extensions['Drupal\Core\Template\TwigExtension']->escapeFilter($this->env, CoreExtension::getAttribute($this->env, $this->source, ($context["attributes"] ?? null), "addClass", ["toolbar", "claro-toolbar"], "method", false, false, true, 26), "html", null, true);
        yield ">
  <nav";
        // line 27
        yield $this->extensions['Drupal\Core\Template\TwigExtension']->escapeFilter($this->env, CoreExtension::getAttribute($this->env, $this->source, ($context["toolbar_attributes"] ?? null), "addClass", ["toolbar-bar", "clearfix"], "method", false, false, true, 27), "html", null, true);
        yield ">
    <h2 class=\"visually-hidden\">";
        // line 28
        yield $this->extensions['Drupal\Core\Template\TwigExtension']->escapeFilter($this->env, ($context["toolbar_heading"] ?? null), "html", null, true);
        yield "</h2>
    ";
        // line 29
        $context['_parent'] = $context;
        $context['_seq'] = CoreExtension::ensureTraversable(($context["tabs"] ?? null));
        foreach ($context['_seq'] as $context["key"] => $context["tab"]) {
            // line 30
            yield "      ";
            $context["tray"] = (($_v0 = ($context["trays"] ?? null)) && is_array($_v0) || $_v0 instanceof ArrayAccess && in_array($_v0::class, CoreExtension::ARRAY_LIKE_CLASSES, true) ? ($_v0[$context["key"]] ?? null) : CoreExtension::getAttribute($this->env, $this->source, ($context["trays"] ?? null), $context["key"], [], "array", false, false, true, 30));
            // line 31
            yield "      <div";
            yield $this->extensions['Drupal\Core\Template\TwigExtension']->escapeFilter($this->env, CoreExtension::getAttribute($this->env, $this->source, CoreExtension::getAttribute($this->env, $this->source, $context["tab"], "attributes", [], "any", false, false, true, 31), "addClass", ["toolbar-tab"], "method", false, false, true, 31), "html", null, true);
            yield ">
        ";
            // line 32
            yield $this->extensions['Drupal\Core\Template\TwigExtension']->escapeFilter($this->env, CoreExtension::getAttribute($this->env, $this->source, $context["tab"], "link", [], "any", false, false, true, 32), "html", null, true);
            yield "
        <div";
            // line 33
            yield $this->extensions['Drupal\Core\Template\TwigExtension']->escapeFilter($this->env, CoreExtension::getAttribute($this->env, $this->source, ($context["tray"] ?? null), "attributes", [], "any", false, false, true, 33), "html", null, true);
            yield ">
          ";
            // line 34
            if ((($tmp = CoreExtension::getAttribute($this->env, $this->source, ($context["tray"] ?? null), "label", [], "any", false, false, true, 34)) && $tmp instanceof Markup ? (string) $tmp : $tmp)) {
                // line 35
                yield "            <nav class=\"toolbar-lining clearfix\" role=\"navigation\" aria-label=\"";
                yield $this->extensions['Drupal\Core\Template\TwigExtension']->escapeFilter($this->env, CoreExtension::getAttribute($this->env, $this->source, ($context["tray"] ?? null), "label", [], "any", false, false, true, 35), "html", null, true);
                yield "\">
              <h3 class=\"toolbar-tray-name visually-hidden\">";
                // line 36
                yield $this->extensions['Drupal\Core\Template\TwigExtension']->escapeFilter($this->env, CoreExtension::getAttribute($this->env, $this->source, ($context["tray"] ?? null), "label", [], "any", false, false, true, 36), "html", null, true);
                yield "</h3>
          ";
            } else {
                // line 38
                yield "            <nav class=\"toolbar-lining clearfix\" role=\"navigation\">
          ";
            }
            // line 40
            yield $this->extensions['Drupal\Core\Template\TwigExtension']->escapeFilter($this->env, CoreExtension::getAttribute($this->env, $this->source, ($context["tray"] ?? null), "links", [], "any", false, false, true, 40), "html", null, true);
            // line 41
            yield "</nav>
        </div>
      </div>
    ";
        }
        $_parent = $context['_parent'];
        unset($context['_seq'], $context['key'], $context['tab'], $context['_parent']);
        $context = array_intersect_key($context, $_parent) + $_parent;
        // line 45
        yield "  </nav>
  ";
        // line 46
        yield $this->extensions['Drupal\Core\Template\TwigExtension']->escapeFilter($this->env, ($context["remainder"] ?? null), "html", null, true);
        yield "
</div>
";
        $this->env->getExtension('\Drupal\Core\Template\TwigExtension')
            ->checkDeprecations($context, ["attributes", "toolbar_attributes", "toolbar_heading", "tabs", "trays", "remainder"]);        yield from [];
    }

    /**
     * @codeCoverageIgnore
     */
    public function getTemplateName(): string
    {
        return "core/themes/claro/templates/navigation/toolbar.html.twig";
    }

    /**
     * @codeCoverageIgnore
     */
    public function isTraitable(): bool
    {
        return false;
    }

    /**
     * @codeCoverageIgnore
     */
    public function getDebugInfo(): array
    {
        return array (  107 => 46,  104 => 45,  95 => 41,  93 => 40,  89 => 38,  84 => 36,  79 => 35,  77 => 34,  73 => 33,  69 => 32,  64 => 31,  61 => 30,  57 => 29,  53 => 28,  49 => 27,  44 => 26,);
    }

    public function getSourceContext(): Source
    {
        return new Source("", "core/themes/claro/templates/navigation/toolbar.html.twig", "/workspaces/unicorninvesting/WebFrontend/web/core/themes/claro/templates/navigation/toolbar.html.twig");
    }
    
    public function checkSecurity()
    {
        static $tags = ["for" => 29, "set" => 30, "if" => 34];
        static $filters = ["escape" => 26];
        static $functions = [];

        try {
            $this->sandbox->checkSecurity(
                ['for', 'set', 'if'],
                ['escape'],
                [],
                $this->source
            );
        } catch (SecurityError $e) {
            $e->setSourceContext($this->source);

            if ($e instanceof SecurityNotAllowedTagError && isset($tags[$e->getTagName()])) {
                $e->setTemplateLine($tags[$e->getTagName()]);
            } elseif ($e instanceof SecurityNotAllowedFilterError && isset($filters[$e->getFilterName()])) {
                $e->setTemplateLine($filters[$e->getFilterName()]);
            } elseif ($e instanceof SecurityNotAllowedFunctionError && isset($functions[$e->getFunctionName()])) {
                $e->setTemplateLine($functions[$e->getFunctionName()]);
            }

            throw $e;
        }

    }
}
