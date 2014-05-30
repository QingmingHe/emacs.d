import jedi
import logging

logger = logging.getLogger(__name__)


def process(attributes, command):
    """Process Jedi operation correspond to request.

    Accepted keywords:
    attributes -- arguments dicitionary passed to Jedi script constructor
    command -- method name called from Anaconda backend
    """

    try:

        anaconda = Anaconda(**attributes)
        logger.debug('Start jedi processing')

        anaconda_method = getattr(anaconda, command)
        logger.debug('Select anaconda method: %s', anaconda_method)

        result = anaconda_method()

    except AttributeError:

        message = 'Call unsupported operation: {0}'.format(command)
        logger.exception(message)
        result = None

    except TypeError:

        message = 'Missing parameters for Jedi object: {0}'.format(attributes)
        logger.exception(message)
        result = None

    # Protection from empty strings, lists, etc.
    # Must return None in all this cases.
    if result:
        return result


class Anaconda():
    """Jedi library interaction."""

    def __init__(self, source, line, column, path):
        """Initialize Jedi with source."""

        self.script = jedi.Script(source, line, column, path)

    def complete(self):
        """Select auto-complete candidates for source position."""

        completions = []

        for comp in self.script.completions():

            completions.append({
                'name': comp.name,
                'doc': comp.doc or None,
                'short_doc': first_line(comp.raw_doc) or None,
            })

        logger.debug('Completions: %s', completions)

        return completions

    def _goto(self):
        """List definitions with assignments.

        Filter same definitions in favor of more explicit definition.
        """

        assignments = self.script.goto_assignments()
        definitions = self.script.goto_definitions()

        for name in assignments + definitions:

            if name.module_path.endswith('.py') and name.type != 'import':

                yield name

    def location(self):
        """Find names assignment place."""

        return dict((summary(name), details(name)) for name in self._goto())

    def reference(self):
        """Find name reference places."""

        usages = self.script.usages()

        locations = self._goto()

        references = [name for name in usages if name not in locations]

        return dict((summary(name), details(name)) for name in references)

    def doc(self):
        """Documentations list for all definitions at point."""

        docs = {}

        for definition in self.script.goto_definitions():

            if definition.raw_doc:

                docs[first_line(definition.raw_doc)] = definition.doc

        return docs

    def eldoc(self):
        """Return eldoc format documentation string or None."""

        signatures = self.script.call_signatures()

        logger.debug('Call signatures: %s', signatures)

        if len(signatures) == 1:

            call_name = signatures[0].call_name

            params = signatures[0].params
            call_params = [param.get_code(new_line=False) for param in params]

            return '{0}({1})'.format(call_name, ', '.join(call_params))


def details(definition):
    """Make hash with definition details."""

    return {
        'module_path': definition.module_path,
        'line': definition.line,
        'column': definition.column,
        'description': definition.description
    }


def summary(definition):
    """Summarize definition into one string."""

    return '{0}:{1} - {2}'.format(
        definition.module_path,
        definition.line,
        definition.description
    )


def first_line(text):
    """Return text first line."""

    return text.split('\n', 1)[0]
